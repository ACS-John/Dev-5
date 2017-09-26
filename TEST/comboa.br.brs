10000 ! Replace test\ComboA.br
10020 ! -----------------------------------------------------------------------
10040   library 'S:\Core\Library': fncomboa,fnacs,fntos,fnfra,fncmdkey
10060 ! library 'S:\Core\Library': fncmbact,fnlbl,fnremove,fntxt,fnflexinit1,fnflexadd1,fncmdset,fnbutton
10080   dim resp$(1)*80, option$(9999)*20,test$*80
10100   dim resp$(30)*80
10120 ! -----------------------------------------------------------------------
20000   let fntos("ComboA")
20020   for a = 1 to 9999
20040     let option$(a) = "Option #"&str$(a)
20060   next a
20080   let filename$="ComboA-tst"
20100   let test$ = "I'm a test of tool tip text.  Does it work?"
20120 ! let fncomboa(filename$,1,1,mat option$,test$)
20140 ! 
20160   mat opt$(3)
20180   let opt$(1)="Regular Collection"
20200   let opt$(2)="Credit Memo"
20220   let opt$(3)="Debit Memo"
20240   let fncomboa("coll_type_rdc",1,27,mat opt$,opt$(1))
20260 ! 
20280   let fncmdkey("Okay",1,1,1)
20300 ! 
20320   let fnacs("ComboA",0,mat resp$,ckey)
20340 ! 
20360   print resp$(1) : let key$ = resp$(1)(32:41) : print key$
