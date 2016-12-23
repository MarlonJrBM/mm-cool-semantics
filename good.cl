class C {
	a : Int <- 9;
	b : Bool;
	init(x : Int, y : Bool) : C {
           {
		a <- x;
		b <- y;
		self;
           }
	};
};

Class Main {
main():C {
	  (new C).init(1,true)
	};
};
