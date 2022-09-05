use bevy_ecs::{action::*, prelude::*};
use bevy_reflect::Reflect;

#[derive(Clone, Reflect)]
struct ActionA {
    number: i32,
}

impl Action for ActionA {
    type DoParam = ();

    type UndoParam = ();

    fn do_action(&self, _: Self::DoParam) {
        println!("Action! {}", self.number);
    }
}

#[derive(Clone, Reflect)]
struct ActionB {
    text: String,
}

impl Action for ActionB {
    type DoParam = ();

    type UndoParam = ();

    fn do_action(&self, _: Self::DoParam) {
        println!("ActionB: {}", self.text);
    }
}

fn main() {
    let mut world = World::new();
    world.insert_resource(Actions::<ActionA>::new());
    world.insert_resource(Actions::<ActionB>::new());    
    
    let mut schedule = Schedule::default();
    
    let mut first = SystemStage::parallel();
    
}
