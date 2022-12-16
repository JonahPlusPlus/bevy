extern crate proc_macro;

mod component;
mod fetch;

use crate::fetch::derive_world_query_impl;
use bevy_macro_utils::{derive_label, get_named_struct_fields, BevyManifest};
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{format_ident, quote};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    spanned::Spanned,
    token::Comma,
    DeriveInput, GenericParam, Ident, Index, LitInt, Meta, MetaList, NestedMeta, Result,
    Token, TypeParam,
};

struct AllTuples {
    macro_ident: Ident,
    start: usize,
    end: usize,
    idents: Vec<Ident>,
}

impl Parse for AllTuples {
    fn parse(input: ParseStream) -> Result<Self> {
        let macro_ident = input.parse::<Ident>()?;
        input.parse::<Comma>()?;
        let start = input.parse::<LitInt>()?.base10_parse()?;
        input.parse::<Comma>()?;
        let end = input.parse::<LitInt>()?.base10_parse()?;
        input.parse::<Comma>()?;
        let mut idents = vec![input.parse::<Ident>()?];
        while input.parse::<Comma>().is_ok() {
            idents.push(input.parse::<Ident>()?);
        }

        Ok(AllTuples {
            macro_ident,
            start,
            end,
            idents,
        })
    }
}

#[proc_macro]
pub fn all_tuples(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as AllTuples);
    let len = input.end - input.start;
    let mut ident_tuples = Vec::with_capacity(len);
    for i in input.start..=input.end {
        let idents = input
            .idents
            .iter()
            .map(|ident| format_ident!("{}{}", ident, i));
        if input.idents.len() < 2 {
            ident_tuples.push(quote! {
                #(#idents)*
            });
        } else {
            ident_tuples.push(quote! {
                (#(#idents),*)
            });
        }
    }

    let macro_ident = &input.macro_ident;
    let invocations = (input.start..=input.end).map(|i| {
        let ident_tuples = &ident_tuples[..i];
        quote! {
            #macro_ident!(#(#ident_tuples),*);
        }
    });
    TokenStream::from(quote! {
        #(
            #invocations
        )*
    })
}

enum BundleFieldKind {
    Component,
    Ignore,
}

const BUNDLE_ATTRIBUTE_NAME: &str = "bundle";
const BUNDLE_ATTRIBUTE_IGNORE_NAME: &str = "ignore";

#[proc_macro_derive(Bundle, attributes(bundle))]
pub fn derive_bundle(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let ecs_path = bevy_ecs_path();

    let named_fields = match get_named_struct_fields(&ast.data) {
        Ok(fields) => &fields.named,
        Err(e) => return e.into_compile_error().into(),
    };

    let mut field_kind = Vec::with_capacity(named_fields.len());

    'field_loop: for field in named_fields.iter() {
        for attr in &field.attrs {
            if attr.path.is_ident(BUNDLE_ATTRIBUTE_NAME) {
                if let Ok(Meta::List(MetaList { nested, .. })) = attr.parse_meta() {
                    if let Some(&NestedMeta::Meta(Meta::Path(ref path))) = nested.first() {
                        if path.is_ident(BUNDLE_ATTRIBUTE_IGNORE_NAME) {
                            field_kind.push(BundleFieldKind::Ignore);
                            continue 'field_loop;
                        }

                        return syn::Error::new(
                            path.span(),
                            format!(
                                "Invalid bundle attribute. Use `{BUNDLE_ATTRIBUTE_IGNORE_NAME}`"
                            ),
                        )
                        .into_compile_error()
                        .into();
                    }

                    return syn::Error::new(attr.span(), format!("Invalid bundle attribute. Use `#[{BUNDLE_ATTRIBUTE_NAME}({BUNDLE_ATTRIBUTE_IGNORE_NAME})]`")).into_compile_error().into();
                }
            }
        }

        field_kind.push(BundleFieldKind::Component);
    }

    let field = named_fields
        .iter()
        .map(|field| field.ident.as_ref().unwrap())
        .collect::<Vec<_>>();
    let field_type = named_fields
        .iter()
        .map(|field| &field.ty)
        .collect::<Vec<_>>();

    let mut field_component_ids = Vec::new();
    let mut field_get_components = Vec::new();
    let mut field_from_components = Vec::new();
    for ((field_type, field_kind), field) in
        field_type.iter().zip(field_kind.iter()).zip(field.iter())
    {
        match field_kind {
            BundleFieldKind::Component => {
                field_component_ids.push(quote! {
                <#field_type as #ecs_path::bundle::Bundle>::component_ids(components, storages, &mut *ids);
                });
                field_get_components.push(quote! {
                    self.#field.get_components(&mut *func);
                });
                field_from_components.push(quote! {
                    #field: <#field_type as #ecs_path::bundle::Bundle>::from_components(ctx, &mut *func),
                });
            }

            BundleFieldKind::Ignore => {
                field_from_components.push(quote! {
                    #field: ::std::default::Default::default(),
                });
            }
        }
    }
    let generics = ast.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let struct_name = &ast.ident;

    TokenStream::from(quote! {
        // SAFETY:
        // - ComponentId is returned in field-definition-order. [from_components] and [get_components] use field-definition-order
        // - `Bundle::get_components` is exactly once for each member. Rely's on the Component -> Bundle implementation to properly pass
        //   the correct `StorageType` into the callback.
        unsafe impl #impl_generics #ecs_path::bundle::Bundle for #struct_name #ty_generics #where_clause {
            fn component_ids(
                components: &mut #ecs_path::component::Components,
                storages: &mut #ecs_path::storage::Storages,
                ids: &mut impl FnMut(#ecs_path::component::ComponentId)
            ){
                #(#field_component_ids)*
            }

            #[allow(unused_variables, non_snake_case)]
            unsafe fn from_components<__T, __F>(ctx: &mut __T, func: &mut __F) -> Self
            where
                __F: FnMut(&mut __T) -> #ecs_path::ptr::OwningPtr<'_>
            {
                Self {
                    #(#field_from_components)*
                }
            }

            #[allow(unused_variables)]
            #[inline]
            fn get_components(
                self,
                func: &mut impl FnMut(#ecs_path::component::StorageType, #ecs_path::ptr::OwningPtr<'_>)
            ) {
                #(#field_get_components)*
            }
        }
    })
}

fn get_idents(fmt_string: fn(usize) -> String, count: usize) -> Vec<Ident> {
    (0..count)
        .map(|i| Ident::new(&fmt_string(i), Span::call_site()))
        .collect::<Vec<Ident>>()
}

#[proc_macro]
pub fn impl_param_set(_input: TokenStream) -> TokenStream {
    let mut tokens = TokenStream::new();
    let max_params = 8;
    let params = get_idents(|i| format!("P{i}"), max_params);
    let params_state = get_idents(|i| format!("PF{i}"), max_params);
    let metas = get_idents(|i| format!("m{i}"), max_params);
    let mut param_fn_muts = Vec::new();
    for (i, param) in params.iter().enumerate() {
        let fn_name = Ident::new(&format!("p{i}"), Span::call_site());
        let index = Index::from(i);
        param_fn_muts.push(quote! {
            pub fn #fn_name<'a>(&'a mut self) -> SystemParamItem<'a, 'a, #param> {
                // SAFETY: systems run without conflicts with other systems.
                // Conflicting params in ParamSet are not accessible at the same time
                // ParamSets are guaranteed to not conflict with other SystemParams
                unsafe {
                    <#param::State as SystemParamState<Infallible>>::get_param(&mut self.param_states.#index, &self.system_meta, self.world, self.change_tick)
                }
            }
        });
    }

    for param_count in 1..=max_params {
        let param = &params[0..param_count];
        let param_state = &params_state[0..param_count];
        let meta = &metas[0..param_count];
        let param_fn_mut = &param_fn_muts[0..param_count];
        tokens.extend(TokenStream::from(quote! {
            impl<'w, 's, #(#param: SystemParam<Infallible>,)*> SystemParam<Infallible> for ParamSet<'w, 's, (#(#param,)*)>
            {
                type State = ParamSetState<(#(#param::State,)*)>;
            }

            // SAFETY: All parameters are constrained to ReadOnlyState, so World is only read

            unsafe impl<'w, 's, #(#param,)*> ReadOnlySystemParam for ParamSet<'w, 's, (#(#param,)*)>
            where #(#param: ReadOnlySystemParam,)*
            { }

            // SAFETY: Relevant parameter ComponentId and ArchetypeComponentId access is applied to SystemMeta. If any ParamState conflicts
            // with any prior access, a panic will occur.

            unsafe impl<#(#param_state: SystemParamState<Infallible>,)*> SystemParamState<Infallible> for ParamSetState<(#(#param_state,)*)>
            {
                type Item<'w, 's> = ParamSet<'w, 's, (#(<#param_state as SystemParamState<Infallible>>::Item::<'w, 's>,)*)>;

                fn init(world: &mut World, system_meta: &mut SystemMeta) -> Self {
                    #(
                        // Pretend to add each param to the system alone, see if it conflicts
                        let mut #meta = system_meta.clone();
                        #meta.component_access_set.clear();
                        #meta.archetype_component_access.clear();
                        #param_state::init(world, &mut #meta);
                        let #param = #param_state::init(world, &mut system_meta.clone());
                    )*
                    #(
                        system_meta
                            .component_access_set
                            .extend(#meta.component_access_set);
                        system_meta
                            .archetype_component_access
                            .extend(&#meta.archetype_component_access);
                    )*
                    ParamSetState((#(#param,)*))
                }

                fn new_archetype(&mut self, archetype: &Archetype, system_meta: &mut SystemMeta) {
                    let (#(#param,)*) = &mut self.0;
                    #(
                        #param.new_archetype(archetype, system_meta);
                    )*
                }

                fn apply(&mut self, system_meta: &SystemMeta, world: &mut World) {
                    self.0.apply(system_meta, world)
                }

                #[inline]
                unsafe fn get_param<'w, 's>(
                    state: &'s mut Self,
                    system_meta: &SystemMeta,
                    world: &'w World,
                    change_tick: u32,
                ) -> Self::Item<'w, 's> {
                    ParamSet {
                        param_states: &mut state.0,
                        system_meta: system_meta.clone(),
                        world,
                        change_tick,
                    }
                }
            }

            impl<'w, 's, #(#param: SystemParam<Infallible>,)*> ParamSet<'w, 's, (#(#param,)*)>
            {

                #(#param_fn_mut)*
            }
        }));
    }

    tokens
}

struct SystemParamFieldAttributes {
    pub ignore: bool,
    pub fallibility: Fallibility,
}

static SYSTEM_PARAM_ATTRIBUTE_NAME: &str = "system_param";
static FALLIBILITY_ATTRIBUTE_NAME: &str = "fallibility";

#[derive(PartialEq, Clone, Copy)]
enum Fallibility {
    Infallible,
    Optional,
    Resultful,
}

/// Implement `SystemParam` to use a struct as a parameter in a system
#[proc_macro_derive(SystemParam, attributes(fallibility, system_param))]
pub fn derive_system_param(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let fields = match get_named_struct_fields(&ast.data) {
        Ok(fields) => &fields.named,
        Err(e) => return e.into_compile_error().into(),
    };
    let path = bevy_ecs_path();

    let struct_name = &ast.ident;
    let state_struct_visibility = &ast.vis;

    let mut fallibility = None;
    let mut error_ty = None;

    // Read struct-level attributes
    for attr in &ast.attrs {
        let Some(attr_ident) = attr.path.get_ident() else { continue };
        if attr_ident == FALLIBILITY_ATTRIBUTE_NAME {
            if fallibility.is_none() {
                syn::custom_keyword!(Infallible);
                syn::custom_keyword!(Optional);
                syn::custom_keyword!(Resultful);
                attr.parse_args_with(|input: ParseStream| {
                    if input.parse::<Option<Infallible>>()?.is_some() {
                        fallibility = Some(Fallibility::Infallible);
                    } else
                    if input.parse::<Option<Optional>>()?.is_some() {
                        fallibility = Some(Fallibility::Optional);
                    } else
                    if input.parse::<Option<Resultful>>()?.is_some() {
                        fallibility = Some(Fallibility::Resultful);
                        let _ = input.parse::<Token![,]>()?;
                        error_ty = Some(input.parse::<Ident>()?);
                    } else {
                        return Err(syn::Error::new_spanned(
                            attr,
                            "Attribute `fallibility` should be `Infallible`, `Optional`, or `Resultful`.",
                        ));
                    }
                    Ok(())
                })
                .expect("Invalid 'fallibility' attribute format.");
            } else {
                return syn::Error::new_spanned(
                    attr,
                    "Multiple struct-level 'fallibility' attributes found.",
                )
                .into_compile_error()
                .into();
            }
        }
    }

    let fallibility = fallibility.unwrap_or(Fallibility::Infallible);

    let infallible = &{
        let mut f = path.clone();
        f.segments.push(format_ident!("system").into());
        f.segments.push(format_ident!("Infallible").into());
        f
    };
    let optional = &{
        let mut f = path.clone();
        f.segments.push(format_ident!("system").into());
        f.segments.push(format_ident!("Optional").into());
        f
    };
    let resultful = &{
        let mut f = path.clone();
        f.segments.push(format_ident!("system").into());
        f.segments.push(format_ident!("Resultful").into());
        f
    };
    let fallible = match fallibility {
        Fallibility::Infallible => infallible,
        Fallibility::Optional => optional,
        Fallibility::Resultful => resultful,
    };

    // Read field-level attributes
    let mut field_attributes = vec!();
    for field in fields {
        let mut attributes = SystemParamFieldAttributes {
            ignore: false,
            fallibility,
        };
        for attr in &field.attrs {
            let Some(attr_ident) = attr.path.get_ident() else { continue };
            if attr_ident == SYSTEM_PARAM_ATTRIBUTE_NAME {
                syn::custom_keyword!(ignore);
                attr.parse_args_with(|input: ParseStream| {
                    if input.parse::<Option<ignore>>()?.is_some() {
                        attributes.ignore = true;
                    }
                    Ok(())
                })
                .expect("Invalid 'system_param' attribute format.");
            } else
            if attr_ident == FALLIBILITY_ATTRIBUTE_NAME {
                syn::custom_keyword!(Infallible);
                syn::custom_keyword!(Optional);
                syn::custom_keyword!(Resultful);
                attr.parse_args_with(|input: ParseStream| {
                    if input.parse::<Option<Infallible>>()?.is_some() {
                        attributes.fallibility = Fallibility::Infallible;
                    } else
                    if input.parse::<Option<Optional>>()?.is_some() {
                        attributes.fallibility = Fallibility::Optional;
                    } else
                    if input.parse::<Option<Resultful>>()?.is_some() {
                        attributes.fallibility = Fallibility::Resultful;
                    }
                    Ok(())
                })
                .expect("Invalid 'fallibility' attribute format.");
            }
        }
        field_attributes.push((field.clone(), attributes));
    }

    let mut fields = Vec::new();
    let mut field_states = Vec::new();
    let mut field_idents = Vec::new();
    let mut field_types = Vec::new();
    
    for (i, (field, attrs)) in field_attributes.iter().enumerate() {
        let field_ident = field.ident.as_ref().unwrap();
        let field_ty = &field.ty;
        let index = Index::from(i);

        field_idents.push(field_ident);

        let field_fallibility = match attrs.fallibility {
            Fallibility::Infallible => infallible,
            Fallibility::Optional => optional,
            Fallibility::Resultful => resultful,
        };

        let rh = if attrs.ignore {
            quote! { #field_ty::default() }
        } else {
            field_types.push(field_ty);
            field_states.push(quote! {
                <#field_ty as SystemParam<#field_fallibility>>::State
            });
            let rh_inner = quote! { <<#field_ty as SystemParam<#field_fallibility>>::State as SystemParamState<#field_fallibility>>::get_param(&mut state.state.#index, system_meta, world, change_tick) };
            match attrs.fallibility {
                Fallibility::Infallible =>  {
                    rh_inner
                },
                Fallibility::Optional => {
                    match fallibility {
                        Fallibility::Infallible => quote! { #rh_inner.unwrap_or_else(|| panic!("SystemParam `{}`: Failed to get field `{}`", stringify!(#struct_name), stringify!(#field_ident))) },
                        Fallibility::Optional => quote! { #rh_inner.unwrap_or_else(|| return None) },
                        Fallibility::Resultful => {
                            let error_ty = error_ty.as_ref().unwrap();
                            quote! { #rh_inner.unwrap_or_else(|| #error_ty::from::<Option<#field_ty>>(None)) }
                        }
                    }
                },
                Fallibility::Resultful => {
                    match fallibility {
                        Fallibility::Infallible => quote! { #rh_inner.unwrap_or_else(|| panic!("SystemParam `{}`: Failed to get field `{}`", stringify!(#struct_name), stringify!(#field_ident))) },
                        Fallibility::Optional => quote! { #rh_inner.unwrap_or_else(|| return None) },
                        Fallibility::Resultful => quote! { #rh_inner.map_error(|err| #error_ty::from::<Option<#field_ty>>(err))? },
                    }
                },
            }
        };

        fields.push(quote! {
            let #field_ident = #rh;
        });
    }

    let generics = ast.generics;

    // Emit an error if there's any unrecognized lifetime names.
    for lt in generics.lifetimes() {
        let ident = &lt.lifetime.ident;
        let w = format_ident!("w");
        let s = format_ident!("s");
        if ident != &w && ident != &s {
            return syn::Error::new_spanned(
                lt,
                r#"invalid lifetime name: expected `'w` or `'s`
 'w -- refers to data stored in the World.
 's -- refers to data stored in the SystemParam's state.'"#,
            )
            .into_compile_error()
            .into();
        }
    }

    let (_impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let lifetimeless_generics: Vec<_> = generics
        .params
        .iter()
        .filter(|g| matches!(g, GenericParam::Type(_)))
        .collect();

    let mut punctuated_generics = Punctuated::<_, Token![,]>::new();
    punctuated_generics.extend(lifetimeless_generics.iter().map(|g| match g {
        GenericParam::Type(g) => GenericParam::Type(TypeParam {
            default: None,
            ..g.clone()
        }),
        _ => unreachable!(),
    }));

    let mut punctuated_generic_idents = Punctuated::<_, Token![,]>::new();
    punctuated_generic_idents.extend(lifetimeless_generics.iter().map(|g| match g {
        GenericParam::Type(g) => &g.ident,
        _ => unreachable!(),
    }));

    // Create a where clause for the `ReadOnlySystemParam` impl.
    // Ensure that each field implements `ReadOnlySystemParam`.
    let mut read_only_generics = generics.clone();
    let read_only_where_clause = read_only_generics.make_where_clause();
    for field_type in &field_types {
        read_only_where_clause
            .predicates
            .push(syn::parse_quote!(#field_type: #path::system::ReadOnlySystemParam));
    }
    
    let returned_struct = quote! {
        #struct_name {
            #(#field_idents,)*
        }
    };

    let (get_param_return_type, get_param_return_value) = match fallibility {
        Fallibility::Infallible => (
            quote! { Self::Item<'w, 's> },
            quote! { #returned_struct }
        ),
        Fallibility::Optional => (
            quote! { Option<Self::Item<'w, 's>> },
            quote! { Some(#returned_struct) }
        ),
        Fallibility::Resultful => (
            quote! { Result<Self::Item<'w, 's>, &'s dyn std::error::Error> },
            quote! { Ok(#returned_struct) }
        ),
    };

    TokenStream::from(quote! {
        // We define the FetchState struct in an anonymous scope to avoid polluting the user namespace.
        // The struct can still be accessed via SystemParam::State, e.g. EventReaderState can be accessed via
        // <EventReader<'static, 'static, T> as SystemParam>::State
        const _: () = {
            use #path::{
                archetype::Archetype,
                system::{
                    SystemParam,
                    SystemParamState,
                    ReadOnlySystemParam,
                    SystemMeta,
                },
                world::World,
            };

            impl<'w, 's, #punctuated_generics> SystemParam<#fallible> for #struct_name #ty_generics #where_clause {
                type State = State<'w, 's, #punctuated_generic_idents>;
            }

            #[doc(hidden)]
            type State<'w, 's, #punctuated_generic_idents> = FetchState<
                (#(#field_states,)*),
                #punctuated_generic_idents
            >;

            #[doc(hidden)]
            #state_struct_visibility struct FetchState <TSystemParamState, #punctuated_generic_idents> {
                state: TSystemParamState,
                marker: std::marker::PhantomData<fn()->(#punctuated_generic_idents)>
            }

            unsafe impl<'__w, '__s, #punctuated_generics> SystemParamState<#fallible> for
                State<'__w, '__s, #punctuated_generic_idents>
            #where_clause {
                type Item<'w, 's> = #struct_name #ty_generics;

                fn init(world: &mut World, system_meta: &mut SystemMeta) -> Self {
                    Self {
                        state: SystemParamState::init(world, system_meta),
                        marker: std::marker::PhantomData,
                    }
                }

                fn new_archetype(&mut self, archetype: &Archetype, system_meta: &mut SystemMeta) {
                    self.state.new_archetype(archetype, system_meta)
                }

                fn apply(&mut self, system_meta: &SystemMeta, world: &mut World) {
                    SystemParamState::apply(&mut self.state, system_meta, world)
                }

                unsafe fn get_param<'w, 's>(
                    state: &'s mut Self,
                    system_meta: &SystemMeta,
                    world: &'w World,
                    change_tick: u32,
                ) -> #get_param_return_type {
                    #(#fields)*

                    #get_param_return_value
                }
            }

            // Safety: Each field is `ReadOnlySystemParam`, so this can only read from the `World`
            unsafe impl<'w, 's, #punctuated_generics> ReadOnlySystemParam for #struct_name #ty_generics #read_only_where_clause {}
        };
    })
}

/// Implement `WorldQuery` to use a struct as a parameter in a query
#[proc_macro_derive(WorldQuery, attributes(world_query))]
pub fn derive_world_query(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    derive_world_query_impl(ast)
}

/// Generates an impl of the `SystemLabel` trait.
///
/// This works only for unit structs, or enums with only unit variants.
/// You may force a struct or variant to behave as if it were fieldless with `#[system_label(ignore_fields)]`.
#[proc_macro_derive(SystemLabel, attributes(system_label))]
pub fn derive_system_label(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let mut trait_path = bevy_ecs_path();
    trait_path.segments.push(format_ident!("schedule").into());
    trait_path
        .segments
        .push(format_ident!("SystemLabel").into());
    derive_label(input, &trait_path, "system_label")
}

/// Generates an impl of the `StageLabel` trait.
///
/// This works only for unit structs, or enums with only unit variants.
/// You may force a struct or variant to behave as if it were fieldless with `#[stage_label(ignore_fields)]`.
#[proc_macro_derive(StageLabel, attributes(stage_label))]
pub fn derive_stage_label(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let mut trait_path = bevy_ecs_path();
    trait_path.segments.push(format_ident!("schedule").into());
    trait_path.segments.push(format_ident!("StageLabel").into());
    derive_label(input, &trait_path, "stage_label")
}

/// Generates an impl of the `RunCriteriaLabel` trait.
///
/// This works only for unit structs, or enums with only unit variants.
/// You may force a struct or variant to behave as if it were fieldless with `#[run_criteria_label(ignore_fields)]`.
#[proc_macro_derive(RunCriteriaLabel, attributes(run_criteria_label))]
pub fn derive_run_criteria_label(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let mut trait_path = bevy_ecs_path();
    trait_path.segments.push(format_ident!("schedule").into());
    trait_path
        .segments
        .push(format_ident!("RunCriteriaLabel").into());
    derive_label(input, &trait_path, "run_criteria_label")
}

pub(crate) fn bevy_ecs_path() -> syn::Path {
    BevyManifest::default().get_path("bevy_ecs")
}

#[proc_macro_derive(Resource)]
pub fn derive_resource(input: TokenStream) -> TokenStream {
    component::derive_resource(input)
}

#[proc_macro_derive(Component, attributes(component))]
pub fn derive_component(input: TokenStream) -> TokenStream {
    component::derive_component(input)
}
