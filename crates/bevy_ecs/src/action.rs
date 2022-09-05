use crate::system::{ResMut, ResMutState, Resource, SystemParam, StaticSystemParam, Local};
use bevy_ecs_macros::{all_tuples, impl_action_set_tuple};
use bevy_reflect::Reflect;
use std::{collections::VecDeque, marker::PhantomData};

pub trait Action: Reflect + Clone {
    type DoParam: SystemParam;
    type UndoParam: SystemParam;

    fn do_action(&self, _: Self::DoParam);
    fn undo_action(&self, _: Self::UndoParam) {}
}

impl<T: Action> SystemParam for T {
    type Fetch = ResMutState<Actions<T>>;
}

pub struct ActionCall<T: Action>(u32, T);

pub struct Actions<T: Action> {
    queue: VecDeque<ActionCall<T>>,
    stack: Vec<ActionCall<T>>,
}

impl<T: Action> Resource for Actions<T> {}

impl<T: Action> Actions<T> {
    pub fn new() -> Self {
        Self {
            queue: VecDeque::new(),
            stack: Vec::new(),
        }
    }

    pub fn insert(&mut self, action: ActionCall<T>) {
        self.queue.push_back(action);
    }
    
    pub fn first_todo() {
        
    }

    pub fn do_action(&mut self, param: T::DoParam) -> bool {
        let action = match self.queue.pop_front() {
            Some(a) => a,
            None => return false,
        };

        action.1.do_action(param);

        self.stack.push(action);

        true
    }
}

pub struct ActionView<S: ActionSet> {
    set: S,
    counter: u32
}

impl<'w, 's, S: ActionSet> ActionView<'w, 's, S> {
    pub fn insert(&mut self, action: impl Action) {
        self.set.insert(action);
    }
}

pub trait ActionSet: SystemParam {
    type DoParamSet: SystemParam;
    
    fn insert(&mut self, action: impl Action);
    
    fn do_action(&mut self);
}

all_tuples!(impl_action_set_tuple, 0, 16, P);
