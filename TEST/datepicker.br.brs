00010 ! S:\acsCL\unpaidinvoice - backup copy in C:\ACS\Dev-5\acsCL\UnpaidInvoice(before_overhaul09-28-2016).br.brs
00012 ! r: SETUP: fntop, dims, open files, etc
00020   library 'S:\Core\Library': fntop,fnxit, fncno,fnopenprn,fncloseprn,fnerror,fntos,fnfra,fnlbl,fntxt,fncombof,fncomboa,fnbutton,fncmdkey,fnacs,fnmsgbox,fnflexadd1,fnflexinit1,fnchk,fnaddpayee,fnagl$,fnrgl$,fnjob_srch,fncmbjob,fncmbcategory
00030   library 'S:\Core\Library': fngethandle,fncmbsubcat,fncategory_srch,fnregistered_for_job_cost_pr,fncmdset,fnrglbig$,fnqglbig
00040   fntop(program$,cap$="Test DatePicker")
00060 ! ______________________________________________________________________
00070   dim cap$*128
00080   dim jobdesc$*30,jn$*6,l(11),ta(2),jobname$*25,jobitem$(6)*30
00090   dim in1$(9),de$*30,ta(2)
00100   dim pr$(4)*30,t1(5),up$(4),unpaidkey$*20
00110   dim d(2),sn$*50
00130   dim jn$*6,cn$*11,l(13)
00150   dim contact$*30,ph$*12,email$*50,fax$*12,myact$*20,resp$(50)*50
00160   dim chdr$(16),cmask$(16),item$(16)*21 ! used with flex grid
00170   dim gldesc$*30,ml$(3)*80
00180   dim item1$(3)*15,type$*25,holdkey$*20,resp$(256)*50
08320 ! ______________________________________________________________________
04815   open #clearing=89: "Name="&env$('Q')&"\CLmstr\clearing.H"&wsid$&",replace,RecL=114",internal,outin,relative  ! kj wrong recl
04889   fntos(sn$="paidinv")
04891   respc=0 : mat resp$=('')
04893   fnlbl(1,1,trim$(env$('cnam')(1:30))&"-"&type$,65,2)
04895              fnflexinit1('unpaidinv',5,27,15,55,mat chdr$,mat cmask$,1)
04897              restore #clearing: 
04899              if nextrec>0 and displayattop$="True" then goto L4890 else goto L5030
04901            L4890: for j=nextrec to lrec(clearing) ! read starting with next record
04903                read #clearing,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8',rec=j: flxitm$(4), flxitm$(5), flxitm$(6),flxitm$(7), flxitm$(8), up$(4),upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof L4940
04905                flxitm$(9)=str$(upa) : flxitm$(10)=str$(disamt)
04907                flxitm$(11)=str$(ddate) : flxitm$(3)=str$(pcde)
04909                flxitm$(2)= flxitm$(12)=str$(bcde): flxitm$(13)=str$(ckn)
04911                flxitm$(14)=str$(dp) : flxitm$(15)=str$(gde)
04913                flxitm$(16)=str$(pdte)
04915                flxitm$(1)=str$(rec(clearing))
04917                if pcde=1 then flxitm$(3)="Yes" else if pcde=0 then flxitm$(3)="No" else if pcde=1 and dp>0 then flxitm$(3)="Paid"
04919            fnflexadd1(mat flxitm$)
04940            L4940: next j
04950            if nextrec=1 then goto L5020 ! thinks it rereads the 1st record twice
04960            for j=1 to max(nextrec-1,1) ! read records previously coded or skipped
04970              read #clearing,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8',rec=j: flxitm$(4), flxitm$(5), flxitm$(6),flxitm$(7), flxitm$(8), up$(4),upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof L5070
04980              flxitm$(9)=str$(upa) : flxitm$(10)=str$(disamt)
04990              flxitm$(11)=str$(ddate) : flxitm$(3)=str$(pcde)
05000              flxitm$(2)= flxitm$(12)=str$(bcde): flxitm$(13)=str$(ckn)
05010              flxitm$(14)=str$(dp) : flxitm$(15)=str$(gde)
05020              flxitm$(1)=str$(rec(clearing))
05030              if pcde=1 then flxitm$(3)="Yes" else if pcde=0 then flxitm$(3)="No" else if pcde=1 and dp>0 then flxitm$(3)="Paid"
05040            fnflexadd1(mat flxitm$)
05050            next j
05055            L5020: goto L5070
05057            L5030: ! 
05059            read #clearing,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8': flxitm$(4), flxitm$(5), flxitm$(6),flxitm$(7), flxitm$(8), up$(4),upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof L5070
05061            flxitm$(9)=str$(upa) : flxitm$(10)=str$(disamt)
05063            flxitm$(11)=str$(ddate) : flxitm$(3)=str$(pcde)
05065            flxitm$(2)= flxitm$(12)=str$(bcde): flxitm$(13)=str$(ckn)
05067            flxitm$(14)=str$(dp) : flxitm$(15)=str$(gde)
05069            flxitm$(16)=str$(pdte)
05071            flxitm$(1)=str$(rec(clearing)) ! assign flxitm$(1) with new record #
05073            if pcde=1 then flxitm$(3)="Yes" else if pcde=0 then flxitm$(3)="No" else if pcde=1 and dp>0 then flxitm$(3)="Paid"
05075            fnflexadd1(mat flxitm$) : goto L5030
05077 L5070: fnfra(2,1,13,23,"Approval Options"," ")
05079 fnbutton(1,2,"&Approve All",62,"Will select to pay all unpaid invoices",1,18,1)
05081 fnbutton(3,2,"&Approve by Range",63,"Enter a range of reference numbers to approve.  The reference # is the number to the left assigned by the computer.",1,18,1)
05083 fnlbl(4,4,"From:",5,1,0,1)
05085 fntxt(4,11,5,0,1,"30",0,"Select the first reference # to be approved",1)
05087 resp$(respc+=1)=""
05089 fnlbl(5,4,"To:",5,1,0,1)
05091 fntxt(5,11,5,0,1,"30",0,"Select the last reference # to be approved",1)
05093 resp$(respc+=1)=""
05095 fnbutton(7,2,"&Approve by Due Date",64,"Approve all invoices due by a certain date.",1,18,1)
05097 fnlbl(8,2,"Date:",5,1,0,1)
05099 fntxt(8,8,8,0,1,"1",0,"All invoices with a due by date equal to or less than this date will be approved",1)
05101 resp$(respc+=1)=""
05103 ! fnBUTTON(10,2,"Approve &Highlighted",65,"Approves one invoice at a time.  Highlight the selected invoice. To remove the approval, use this option and highlight the same invoice second time.",1,18,1,0,1)
05105 fnbutton(10,2,"Approve By Payee",66,"Approves all invoices with this payee number in invoice record.",1,18,1)
05107 fnlbl(11,2,"Payee #:",8,1,0,1)
05109 fntxt(11,11,8,0,1,"",0,"Enter payee # to approve all invoices on that payee",1)
05111 resp$(respc+=1)=""
05113 if displayunpaid=1 or displayunpaid=0 then wording$="Total Selected:" else wording$= "Total Unapproved:"
05115 fnlbl(2,28,wording$,18,1)
05117 fntxt(2,49,12,0,1,"10",0," ")
05119 resp$(respc+=1)=str$(total)
05121 fnchk(3,47,"Display at Top:",1)
05123 resp$(respc+=1)=displayattop$
05125 fncmdkey("&Approve Highlighted",1,1,0,"Approves or cancels the invoice that is highlighted.")
05127 fncmdkey("&Display All",9,0,0,"Displays all remaining records in the unpaid file.")
05129 fncmdkey("&Display Selected",3,0,0,"Displays all invoices selected for payment")
05131 fncmdkey("&Display UnSelected",2,0,0,"Displays all remaining uncleared invoices")
05133 fncmdkey("C&omplete",5,0,1,"Return to main unpaid invoice menu")
05135 fnacs(sn$,0,mat resp$,ck)