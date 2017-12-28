10000 ! Replace test\ComboA.br
10020 ! -----------------------------------------------------------------------
10040   library 'S:\Core\Library': fncomboa,fnAcs,fnTos,fnFra,fnCmdKey
10060 ! library 'S:\Core\Library': fncmbact,fnLbl,fnremove,fnTxt,fnflexinit1,fnflexadd1,fnCmdSet,fnButton
10080   dim resp$(1)*80, option$(9999)*20,test$*80
10100   dim resp$(30)*80
10120 ! -----------------------------------------------------------------------
20000   fnTos("ComboA")
20020   for a = 1 to 9999
20040     option$(a) = "Option #"&str$(a)
20060   next a
20080   filename$="ComboA-tst"
20100   test$ = "I'm a test of tool tip text.  Does it work?"
20120 ! fncomboa(filename$,1,1,mat option$,test$)
20140 ! 
20160   mat opt$(3)
20180   opt$(1)="Regular Collection"
20200   opt$(2)="Credit Memo"
20220   opt$(3)="Debit Memo"
20240   fncomboa("coll_type_rdc",1,27,mat opt$,opt$(1))
20260 ! 
20280   fnCmdKey("Okay",1,1,1)
20300 ! 
20320   fnAcs("ComboA",0,mat resp$,ckey)
20340 ! 
20360   pr resp$(1) : key$ = resp$(1)(32:41) : pr key$
