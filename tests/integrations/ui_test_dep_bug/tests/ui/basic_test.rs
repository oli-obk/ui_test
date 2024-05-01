use tested_crate::B;

struct DoesntImplementA;

impl B for DoesntImplementA {}
//~^ E0277