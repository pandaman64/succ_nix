mod attr_set;
mod boolean;
mod fun;
mod integer;
mod string;
mod var;

pub use attr_set::{AttrSetDomain, AttrSetType};
pub use boolean::BoolDomain;
pub use fun::{FunDomain, FunType};
pub use integer::IntDomain;
pub use string::StringDomain;
pub use var::VarDomain;
