! Replace test\ComboA.br
! -----------------------------------------------------------------------
	autoLibrary
	dim resp$(1)*80, option$(9999)*20,test$*80
	dim resp$(30)*80
! -----------------------------------------------------------------------
	fnTos("ComboA")
	for a = 1 to 9999
		option$(a) = "Option #"&str$(a)
	next a
	filename$="ComboA-tst"
	test$ = "I'm a test of tool tip text.  Does it work?"
! fncomboa(filename$,1,1,mat option$,test$)
!
	mat opt$(3)
	opt$(1)="Regular Collection"
	opt$(2)="Credit Memo"
	opt$(3)="Debit Memo"
	fncomboa("coll_type_rdc",1,27,mat opt$,opt$(1))
!
	fnCmdKey("Okay",1,1,1)
!
	fnAcs("ComboA",0,mat resp$,ckey)
!
	pr resp$(1) : key$ = resp$(1)(32:41) : pr key$
