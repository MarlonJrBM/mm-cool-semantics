class C {
	a : Int;
	b : Bool <- true;
	init(x : Int, y : Bool) : C {
           {
		a <- y;
		b <- y;
		self;
           }
	};
};

Class Main {
	main():C {
	 {
	  (new C).init(1,1); (* calling method with wrong signature  *)
	  (new C).init(1,true,3); (* same as above *)
	  (new C).iinit(1,true,3); (* calling method that doesn't exist *)
	  (new C);
      (new Elis); (* instantiating type that doesn't exist *)
	 }
	};
};


(* cyle in inheritance graph 
class A inherits E {};
class B inherits A {};
class Ca inherits B {};
class D inherits Ca {};
class E inherits D  {};
class F inherits FF {};*)
