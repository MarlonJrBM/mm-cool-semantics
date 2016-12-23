class C {
	a : Int <- 9;
	b : Bool;
    c : SELF_TYPE;
	init(x : Int, y : Bool) : C {
           {
		a <- x;
		b <- y;
		self;
           }
	};

    foo() : SELF_TYPE {
        new SELF_TYPE
    };
};

Class Main {
main():C {
	  (new C).init(1,true)
	};
};
