use crate::system::{ResMut, ResMutState, Resource, SystemParam, StaticSystemParam, SystemParamItem};
use bevy_ecs_macros::{all_tuples, impl_action_set_tuple};
use bevy_reflect::Reflect;
use std::{collections::VecDeque, ops::DerefMut};

pub trait Action: Reflect + Clone {
    type DoParam: SystemParam;
    type UndoParam: SystemParam;

    fn do_action(&self, _: Self::DoParam);
    fn undo_action(&self, _: Self::UndoParam) {}
}

impl<T: Action> SystemParam for T {
    type Fetch = ResMutState<Actions<T>>;
}

pub struct ActionCall<T: Action>(u64, T);

#[derive(Resource)]
pub struct Actions<T: Action> {
    queue: VecDeque<ActionCall<T>>,
    stack: Vec<ActionCall<T>>,
}

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
    
    pub fn first_todo(&mut self) -> u32 {
        0
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

use crate as bevy_ecs;

#[derive(Resource)]
pub struct ActionCallCounter(u64);

impl ActionCallCounter {
    pub fn incr(&mut self) -> u64 {
        let ActionCallCounter(c) = self;
        *c += 1;
        return *c;
    }
}

#[derive(SystemParam)]
pub struct ActionView<'w, 's, S: ActionSet + 'static> {
    actions: StaticSystemParam<'w, 's, S::ActionsFetch>,
    do_params: StaticSystemParam<'w, 's, S::DoParamSet>,
    counter: ResMut<'w, ActionCallCounter>,
}

impl<'w, 's, S: ActionSet + 'static> ActionView<'w, 's, S> {
    pub fn insert(&mut self, action: impl Action) {
        S::insert(action, self.actions.deref_mut(), self.counter.deref_mut());
    }
    
    pub fn do_action(&mut self) {
        S::do_action(self.actions.deref_mut(), self.do_params.deref_mut()); 
    }
}

pub trait ActionSet {
    type ActionsFetch: SystemParam + 'static;
    type DoParamSet: SystemParam + 'static;
    
    fn insert(action: impl Action, actions: &mut SystemParamItem<Self::ActionsFetch>, counter: &mut ActionCallCounter);
    
    fn do_action(actions: &mut SystemParamItem<Self::ActionsFetch>, params: &mut SystemParamItem<Self::DoParamSet>);
}

all_tuples!(impl_action_set_tuple, 0, 16, P);
