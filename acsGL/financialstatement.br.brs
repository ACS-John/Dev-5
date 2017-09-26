10000 ! Replace S:\acsGL\financialstatement
10200 ! ______________________________________________________________________
10300   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fntos,fnfra,fnopt,fncmdkey,fnacs,fndat,fnflexinit1,fnflexadd1,fnlbl,fntxt,fnchk,fncomboa,fnmsgbox
10500   on error goto ERTN
10600 ! ______________________________________________________________________
10700   dim ac(9),rno$*5,d$*50
10800   dim id$(6)*40,fil$(6)*18,idx$(6)*18,cnam$*40,dat$*20
10900   dim te$*1,ml$(4)*80
11000   dim resp$(25)*60
11100   dim cap$*128,item$(24)*50,option2$(10)*55
11200   dim choice$*55
11300 ! ______________________________________________________________________
11400   let fntop("S:\acsGL\financialstatement",cap$="Financial Statement Design")
11500   let fncno(cno,cnam$)
11600   let fndat(dat$)
11700   let fn_field_labels ! get column headers
11800   let hp1=66-int(len(rtrm$(cnam$))/2)
11900   let hp2=66-int(len(rtrm$(dat$))/2)
12000   let id$(1)=" 1. Balance Sheet File" : let fil$(1)="ACGLFNSB.H"&str$(cno) : let idx$(1)="FNSBINDX.H"&str$(cno)
12100   let id$(2)=" 2. Income Statement File" : let fil$(2)="ACGLFNSI.H"&str$(cno) : let idx$(2)="FNSIINDX.H"&str$(cno)
12200   let id$(3)=" 3. Fund Statement / Cash Flow File" : let fil$(3)="ACGLFNSF.H"&str$(cno) : let idx$(3)="FNSFINDX.H"&str$(cno)
12300   let id$(4)=" 4. Secondary Balance Sheet File" : let fil$(4)="ACGLFNSC.H"&str$(cno) : let idx$(4)="FNSCINDX.H"&str$(cno)
12400   let id$(5)=" 5. Secondary Income Statement File" : let fil$(5)="ACGLFNSJ.H"&str$(cno) : let idx$(5)="FNSJINDX.H"&str$(cno)
12500   let id$(6)=" 6. Secondary Fund / Cash Flow File" : let fil$(6)="ACGLFNSG.H"&str$(cno) : let idx$(6)="FNSGINDX.H"&str$(cno)
12600 ! ______________________________________________________________________
12700 MENU1: ! 
12800   let fntos(sn$="FsDesign")
12900   let mylen=20: let mypos=mylen+3 : let right=1
13000   let fnfra(1,1,6,60,"Financial Statement Choices","Choose the financial statement to work with.")
13100   let fnopt(1,2,id$(1),0,1)
13200   let resp$(1)="True"
13300   let fnopt(2,2,id$(2) ,0,1)
13400   let resp$(2)="False"
13500   let fnopt(3,2,id$(3),0,1)
13600   let resp$(3)="False"
13700   let fnopt(4,2,id$(4),0,1)
13800   let resp$(4)="False"
13900   let fnopt(5,2,id$(5),0,1)
14000   let resp$(5)="False"
14100   let fnopt(6,2,id$(6),0,1)
14200   let resp$(6)="False"
14300   let f1=curfld
14400   let fncmdkey("&Next",1,1,0,"Access the chosen financial statement design.")
14500   let fncmdkey("&Build D Records",2,0,0,"Allows you to build all type D records automatically without having to rekey all descriptions.")
14600   let fncmdkey("&Proof List",3,0,0,"Allows you to print a proof list of the financial statement layout.")
14700   let fncmdkey("&Cancel",5,0,1,"Return to main menu.")
14800   let fnacs(sn$,0,mat resp$,ckey)
14900   if ckey=5 then goto XIT
15000   if ckey=2 then chain "S:\acsGL\Bld_D_Records"
15100   if resp$(1)="True" then let selection=1
15200   if resp$(2)="True" then let selection=2
15300   if resp$(3)="True" then let selection=3
15400   if resp$(4)="True" then let selection=4
15500   if resp$(5)="True" then let selection=5
15600   if resp$(6)="True" then let selection=6
15700   let f1=selection
15800   close #1: ioerr L520
15900 L520: open #fin_stmt=1: "Name="&env$('Q')&"\GLmstr\"&fil$(f1)&",KFName="&env$('Q')&"\GLmstr\"&idx$(f1)&",Shr",internal,outin,keyed ioerr MENU1
16000   if ckey=3 then let fn_print_proof
16100 FIN_STMT_GRID: ! 
16200   let fntos(sn$="fin_stmt")
16300   let fnflexinit1('fin_stmtgl',lc=1,1,10,70,mat chdr$,mat cmask$,1)
16400   restore #fin_stmt: 
16500 READ_FIN_STMT: ! read fin_stmt file
16600 L600: read #fin_stmt,using "Form POS 1,C 5,C 50,C 1,2*N 2,15*N 1,N 3,N 5": rno$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,rnp norec L630,eof EO_FIN_STMT_GRID,conv FIXRNP
16700   let item$(1)=str$(rec(fin_stmt))
16800   let item$(2)=rno$: let item$(3)=d$: let item$(4)=te$
16900   let item$(5)=str$(sp) : let item$(6)=str$(ls) : let item$(7)=str$(ds)
17000   let item$(8)=str$(ul) : let item$(9)=str$(rs) : let item$(10)=str$(bc)
17100   let item$(11)=str$(ap) : let item$(12)=str$(ac(1)) : let item$(13)=str$(ac(2))
17200   let item$(14)=str$(ac(3)) : let item$(15)=str$(ac(4)) : let item$(16)=str$(ac(5))
17300   let item$(17)=str$(ac(6)) : let item$(18)=str$(ac(7)) : let item$(19)=str$(ac(8))
17400   let item$(20)=str$(ac(9)) : let item$(21)=str$(rnp) : let item$(22)=str$(fc)
17500   let item$(23)=d$(1:10)
17600   let fnflexadd1(mat item$)
17700 L630: goto READ_FIN_STMT
17800 EO_FIN_STMT_GRID: ! 
17900   let fncmdkey("&Edit",2,1,0,"Highlight any record and press Enter or click Edit to change any existing financial statement reference number.")
18000   let fncmdkey("&Add",1,0,0,"Allows you to add new financial statement reference numbers.")
18100   let fncmdkey("&Delete",8,0,0,"Highlight any record and click Delete to remove the financial statement reference number.")
18200 ! Let FNCMDKEY("&Print",3,0,0,"Takes you directly to the Print financial statement reference number option")
18300   let fncmdkey("E&xit",5,0,1,"Exits to main menu")
18400   let fnacs(sn$,0,mat resp$,ckey)
18500   if ckey=5 then goto L2350
18600   let add=edit=0
18700   let editrec=val(resp$(1))
18800   if ckey=1 then 
18900     let add=1
19000     let sp=ls=ds=ul=rs=bc=ap=ic=fc=rnp=0
19100     let rno$=d$=te$=""
19200     mat ac=(0)
19300     goto ADD_EDIT_FIN_STMTS ! add
19400   else if ckey=2 then 
19500     let edit=1
19600     read #fin_stmt,using "Form POS 1,C 5,C 50,C 1,2*N 2,15*N 1,N 3,N 5",rec=editrec: rno$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,rnp norec FIN_STMT_GRID
19700     let holdrno$=rno$
19800     goto ADD_EDIT_FIN_STMTS
19900   else if ckey=8 then 
20000     read #fin_stmt,using "Form POS 1,C 5,C 50,C 1,2*N 2,15*N 1,N 3,N 5",rec=editrec,release: rno$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,rnp norec FIN_STMT_GRID
20100     gosub DELETEIT
20200     goto FIN_STMT_GRID
20300   end if 
20400   goto FIN_STMT_GRID
20500 ADD_EDIT_FIN_STMTS: ! 
20600   let fntos(sn$="fin_stmt1")
20700   let mylen=25: let mypos=mylen+3 : let right=1
20800   let fnlbl(1,1,"F/S Number:",mylen,right)
20900   let fntxt(1,mypos,5,0,right,"30",0,"",0 )
21000   let resp$(1)=rno$
21100   let fnlbl(2,1,"Description::",mylen,right)
21200   let fntxt(2,mypos,50,0,left,"",0,"",0 )
21300   let resp$(2)=d$
21400   if trim$(te$)='' or uprc$(te$)="D" then let choice$=option2$(1) : goto L875
21500   if uprc$(te$)="T" then let choice$=option2$(2) : goto L875
21600   if uprc$(te$)="R" then let choice$=option2$(3) : goto L875
21700   if uprc$(te$)="H" then let choice$=option2$(4) : goto L875
21800   if uprc$(te$)="S" then let choice$=option2$(5) : goto L875
21900   if uprc$(te$)="E" then let choice$=option2$(6) : goto L875
22000   if uprc$(te$)="P" then let choice$=option2$(7) : goto L875
22100   if uprc$(te$)="E" then let choice$=option2$(8) : goto L875
22200   if uprc$(te$)="B" then let choice$=option2$(9) : goto L875
22300   if uprc$(te$)="C" then let choice$=option2$(10) : goto L875
22400 L875: let fnlbl(3,1,"Type of Entry:",mylen,right)
22500   let fncomboa("TypeOfEntry",3,mypos,mat option2$,"Each entry must have a type of transaction.",60)
22600   let resp$(3)=choice$
22700   let fnlbl(4,1,"Starting Print Position:",mylen,right)
22800   let fntxt(4,mypos,3,0,0,"30",0,"Number of spaces to indent.",0 )
22900   let resp$(4)=str$(sp)
23000   let fnlbl(5,1,"Lines to Skip:",mylen,right)
23100   let fntxt(5,mypos,2,0,0,"30",0,"Number of blank lines following this line.",0 )
23200   let resp$(5)=str$(ls)
23300   let fnchk(6,mypos,"Dollar Sign:",1)
23400   if ds=1 then let resp$(6)="True" else let resp$(6)="False"
23500   let fnlbl(7,1,"Underlines:",mylen,right)
23600   let fntxt(7,mypos,1,0,0,"30",0,"Number of under lines following the amount.",0 )
23700   let resp$(7)=str$(ul)
23800   let fnchk(8,mypos,"Reverse Sign:",1)
23900   if rs=1 then let resp$(8)="True" else let resp$(8)="False"
24000   let fnlbl(9,1,"Balance Sheet Column:",mylen,right)
24100   let fntxt(9,mypos,1,0,0,"30",0,"One of three columns available.",0 )
24200   let resp$(9)=str$(bc)
24300   let fnlbl(10,1,"Accumulator to Print:",mylen,right)
24400   let fntxt(10,mypos,1,0,0,"30",0,"One of nine sets of totals available for total records.",0 )
24500   let resp$(10)=str$(ap)
24600   for j=1 to 9
24700     let fnlbl(10+j,1,"Clear Accumulator # "&str$(j)&":",mylen,right)
24800     let fntxt(10+j,mypos,1,0,0,"30",0,"Place a one by each accumulator that should be cleared after this line is printed.",0 )
24900     let resp$(10+j)=str$(ac(j))
25000   next j
25100   let fnlbl(20,1,"Base Item for %:",mylen,right)
25200   let fntxt(20,mypos,5,0,0,"30",0,"Enter the reference # of the line that should be used in calculating this percent.",0 )
25300   let resp$(20)=str$(rnp)
25400   let fnlbl(21,1,"Cost Center Code:",mylen,right)
25500   let fntxt(21,mypos,3,0,0,"30",0,"Enter the fund number for ability to print one fund at a time.",0 )
25600   let resp$(21)=str$(fc)
25700   let fncmdkey("&Save",1,1,0,"Saves changes.")
25800   let fncmdkey("&Cancel",5,0,1,"Returns to list of fin_stmts withouit saving any changes.")
25900   let fnacs(sn$,0,mat resp$,ckey)
26000   if ckey=5 then goto FIN_STMT_GRID
26100   let rno$=resp$(1)
26200   let rno$=lpad$(rtrm$(rno$),5)
26300   let d$=resp$(2)
26400   let te$=resp$(3)(1:1)
26500   let sp=val(resp$(4))
26600   let ls=val(resp$(5))
26700   if resp$(6)="True" then let ds=1 else let ds=0 ! dollar sign
26800   let ul=val(resp$(7))
26900   if resp$(8)="True" then let rs=1 else let rs=0 ! reverse sign
27000   let bc=val(resp$(9))
27100   let ap=val(resp$(10))
27200   for j=1 to 9
27300     let ac(j)=val(resp$(j+10))
27400   next j
27500   let rnp=val(resp$(20)) ! was ic but ic not big enough; moved down
27600   let fc=val(resp$(21))
27700   gosub EDIT_CHECKS
27800   if edit=1 then goto REWRITE_EXISTING_FIN_STMT
27900   if add=1 then goto WRITE_NEW_FIN_STMT
28000   goto FIN_STMT_GRID
28100 ! ______________________________________________________________________
28200 REWRITE_EXISTING_FIN_STMT: ! 
28300   if trim$(rno$)="" or trim$(rno$)="0" then goto ADD_EDIT_FIN_STMTS
28400   if holdrno$<>rno$ and trim$(holdrno$)<>"" then goto MSGBOX1 else goto L1420
28500 MSGBOX1: ! 
28600   mat ml$(3)
28700   let ml$(1)="You are changing reference # "&holdrno$&" to "
28800   let ml$(2)="reference # "&rno$&".  Click OK to continue, else"
28900   let ml$(3)="Cancel to prevent changing the #."
29000   let fnmsgbox(mat ml$,resp$,cap$,49)
29100   if resp$="OK" then goto L1420 else goto ADD_EDIT_FIN_STMTS
29200 L1420: ! 
29300   rewrite #fin_stmt,using L1560,rec=editrec: rno$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,rnp
29400   goto L1470
29500 ! 
29600 WRITE_NEW_FIN_STMT: ! 
29700   write #fin_stmt,using L1560: rno$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,rnp
29800 L1470: ! 
29900   goto FIN_STMT_GRID
30000 ! ______________________________________________________________________
30100 DELETEIT: !  delete a reference #
30200   mat ml$(3)
30300   let ml$(1)="You are attempting to delete reference # "&rno$&"."
30400   let ml$(2)="Click OK to continue, "
30500   let ml$(3)="else Cancel to prevent deleting the reference #."
30600   let fnmsgbox(mat ml$,resp$,cap$,49)
30700   if uprc$(resp$)="OK" then goto L1530 else goto ADD_EDIT_FIN_STMTS
30800 L1530: delete #fin_stmt,rec=editrec: 
30900   goto FIN_STMT_GRID
31000 ! ______________________________________________________________________
31100 L1560: form pos 1,c 5,c 50,c 1,2*n 2,15*n 1,n 3,n 5
31200 ! ______________________________________________________________________
31300 EDIT_CHECKS: ! 
31400   let rno$=lpad$(rtrm$(rno$),5)
31500   let te$=uprc$(te$)
31600   if add=1 and ltrm$(rtrm$(rno$))="" then goto L1640
31700   if add=1 and ltrm$(rtrm$(rno$))="0" then goto L1640
31800   goto L1660
31900 L1640: mat ml$(2)
32000   let ml$(1)="You must have a valid reference number in order to add a new line."
32100   let ml$(2)="Click OK to fix."
32200   let fnmsgbox(mat ml$,resp$,cap$,49)
32300   goto ADD_EDIT_FIN_STMTS
32400 L1660: on pos ("DTRHSFPEBC",te$,1) goto L1690,L1690,L1690,L1690,L1690,L1690,L1690,L1690,L1690,L1690 none L1670
32500 L1670: mat ml$(4)
32600   let ml$(1)="Valid codes include: D=Detail; T=Total; R=Report Heading"
32700   let ml$(2)="H=Heading; S=Secondary Heading; F=Footnote; P=Profit or Loss"
32800   let ml$(3)="E=Something; B=Bank Account; C=Something"
32900   let ml$(4)="Click OK to fix."
33000   let fnmsgbox(mat ml$,resp$,cap$,49)
33100   goto ADD_EDIT_FIN_STMTS
33200 L1690: if ds>=0 and ds<=1 then goto L1720
33300   mat ml$(3)
33400   let ml$(1)="The Dollar Sign codes must be 0 or 1.  0 is no dollar"
33500   let ml$(2)="sign.  1 will print a dollar sign beside the entry."
33600   let ml$(3)="Click OK to fix."
33700   let fnmsgbox(mat ml$,resp$,cap$,49)
33800   goto ADD_EDIT_FIN_STMTS
33900 L1720: if ul>=0 and ul<=2 then goto L1750
34000   mat ml$(3)
34100   let ml$(1)="The Underline Codes must be 0,1 or 2.  0 is no underline."
34200   let ml$(2)="1 will print 1 underline. 2 will print 2 underlines."
34300   let ml$(3)="Click OK to fix."
34400   let fnmsgbox(mat ml$,resp$,cap$,49)
34500   goto ADD_EDIT_FIN_STMTS
34600 L1750: if rs>=0 and rs<=1 then goto L1780
34700   mat ml$(3)
34800   let ml$(1)="The Reverse Sign code is simply 1 to reverse the sign."
34900   let ml$(2)="0 will default to no action. All other codes are invalid."
35000   let ml$(3)="Click OK to fix."
35100   let fnmsgbox(mat ml$,resp$,cap$,49)
35200   goto ADD_EDIT_FIN_STMTS
35300 L1780: if bc>=0 and bc<=3 then goto L1810
35400   mat ml$(3)
35500   let ml$(1)="The Balance Sheet Column is only applicable in a balance"
35600   let ml$(2)="sheet design.  The codes are 0,1,2,3. 0 will default to 1."
35700   let ml$(3)="Click OK to fix."
35800   let fnmsgbox(mat ml$,resp$,cap$,49)
35900   goto ADD_EDIT_FIN_STMTS
36000 L1810: if te$="E" and (ap<1 or ap>9) then let 1850 else goto L1840
36100   mat ml$(3)
36200   let ml$(1)="If the transaction type is an 'E' then you must"
36300   let ml$(2)="enter a valid accumulator to print of 1 thru 9. "
36400   let ml$(3)="Click OK to fix."
36500   let fnmsgbox(mat ml$,resp$,cap$,49)
36600   goto ADD_EDIT_FIN_STMTS
36700 L1840: if ap>=0 and ap<=9 then goto L1870
36800   mat ml$(3)
36900   let ml$(1)="The Accumulator to Print must be no less than 0 or "
37000   let ml$(2)="or no greater than 9. All other codes are invalid, "
37100   let ml$(3)="Click OK to fix."
37200   let fnmsgbox(mat ml$,resp$,cap$,49)
37300   goto ADD_EDIT_FIN_STMTS
37400 L1870: for j=1 to 9
37500     if ac(j)=0 or ac(j)=1 or ac(j)=9 then goto L1910
37600     mat ml$(3)
37700     let ml$(1)="Accumulator to clear must be a 0,1, or 9."
37800     let ml$(2)="All other codes are invalid, "
37900     let ml$(3)="Click OK to fix."
38000     let fnmsgbox(mat ml$,resp$,cap$,49)
38100     goto ADD_EDIT_FIN_STMTS
38200 L1910: next j
38300   return 
38400   def fn_print_proof
38500     restore #fin_stmt,key>="     ": eof FIN_STMT_GRID nokey PRINT_PROOF_XIT
38600     let fnopenprn
38700     let fn_print_proof_hdr
38800     do 
38900       read #fin_stmt,using "Form POS 1,C 5,C 50,C 1,2*N 2,15*N 1,N 3,N 5": rno$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,rnp eof PRINT_PROOF_FINIS
39000       print #255,using L2030: rno$,d$,te$,sp,ls,ds,ul,rs,bc,ap,ac(1),ac(2),ac(3),ac(4),ac(5),ac(6),ac(7),ac(8),ac(9),rnp,fc pageoflow NEWPGE
39100 L2030: form pos 1,c 5,x 1,c 50,c 1,x 2,pic(zzzzz),x 1,pic(zzzzz),x 1,pic(zzzz),x 1,pic(zzzzz),x 1,pic(zzzz),x 1,pic(zzz),x 1,pic(zzzzz),x 1,pic(zzz),pic(zzz),pic(zzz),pic(zzz),pic(zzz),pic(zzz),pic(zzz),pic(zzz),pic(zzz),pic(zzzz),x 1,pic(zzzz),skip 1
39200     loop 
39300 PRINT_PROOF_FINIS: ! 
39400     let fncloseprn
39500 PRINT_PROOF_XIT: ! 
39600   fnend 
39700 NEWPGE: print #255: newpage
39800   let fn_print_proof_hdr
39900   continue 
40000   return 
40100   def fn_print_proof_hdr
40200     print #255,using L2110: date$('mm/dd/yy'),cnam$
40300 L2110: form skip 2,pos 1,c 8,pos hp1,c 40,skip 1
40400     print #255,using L2130: time$,"Financial Statement Layout Proof List -",id$(f1)(5:32),dat$
40500 L2130: form pos 1,c 8,pos 38,c 39,c 32,skip 1,pos hp2,c 20,skip 2
40600     print #255,using L2150: "Type Of","Start","Lines","$","Under","Rev","Bs","# To","*** Clear Accumulator ***","Base","CC"
40700 L2150: form pos 51,c 7,x 2,c 5,x 1,c 5,x 4,c 1,x 1,c 5,x 2,c 3,x 2,c 2,x 2,c 4,x 2,c 25,x 1,c 4,x 2,c 2,skip 1
40800     print #255,using L2170: "Ref #","Description","Entry","Print","Skip","Sign","Line","Sign","Col","Print","1","2","3","4","5","6","7","8","9","Item","Code"
40900 L2170: form pos 1,c 5,x 19,c 11,x 16,c 5,x 3,c 5,x 2,c 4,x 1,c 4,x 2,c 4,x 1,c 4,x 1,c 3,x 1,c 5,x 2,c 1,x 2,c 1,x 2,c 1,x 2,c 1,x 2,c 1,x 2,c 1,x 2,c 1,x 2,c 1,x 2,c 1,x 1,c 4,x 1,c 4,skip 2
41000   fnend 
41100 ! ______________________________________________________________________
41200 XIT: let fnxit
41300 ! ______________________________________________________________________
41400 ! <Updateable Region: ERTN>
41500 ERTN: let fnerror(program$,err,line,act$,"xit")
41600   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
41700   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
41800   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
41900 ERTN_EXEC_ACT: execute act$ : goto ERTN
42000 ! /region
42100 ! ______________________________________________________________________
42200 ! Initial build all financial statements
42300   close #1: ioerr L2320
42400 L2320: open #fin_stmt=1: "Name="&env$('Q')&"\GLmstr\"&fil$(f1)&",NoShr",internal,output 
42500   close #fin_stmt,free: 
42600   open #fin_stmt: "Name="&env$('Q')&"\GLmstr\"&fil$(f1)&",SIZE=0,RecL=83",internal,output 
42700 L2350: for j=1 to 6
42800     close #fin_stmt: ioerr L2370
42900 L2370: execute "Index "&env$('Q')&"\GLmstr\"&fil$(j)&' '&env$('Q')&"\GLmstr\"&idx$(j)&" 1 5 Replace DupKeys" ioerr L2380
43000 L2380: next j
43100   goto MENU1
43200 ! ______________________________________________________________________
43300   def fn_field_labels ! ** Field Labels    **
43400     let ic=0 ! temporary Item Counter
43500     mat chdr$(24) : mat cmask$(24) : mat flxitm$(24)
43600     let chdr$(ic+=1)="Ref #"
43700     let chdr$(ic+=1)="F/S #"
43800     let chdr$(ic+=1)="Description"
43900     let chdr$(ic+=1)="Type"
44000     let chdr$(ic+=1)="SP"
44100     let chdr$(ic+=1)="LS"
44200     let chdr$(ic+=1)="$s"
44300     let chdr$(ic+=1)="UL"
44400     let chdr$(ic+=1)="RS"
44500     let chdr$(ic+=1)="B/S"
44600     let chdr$(ic+=1)="PA"
44700     let chdr$(ic+=1)="C1"
44800     let chdr$(ic+=1)="C2"
44900     let chdr$(ic+=1)="C3"
45000     let chdr$(ic+=1)="C4"
45100     let chdr$(ic+=1)="C5"
45200     let chdr$(ic+=1)="C6"
45300     let chdr$(ic+=1)="C7"
45400     let chdr$(ic+=1)="C8"
45500     let chdr$(ic+=1)="C9"
45600     let chdr$(ic+=1)="Base%"
45700     let chdr$(ic+=1)="CC"
45800     let chdr$(ic+=1)="Abbr Name"
45900 ! ** Field Masks **
46000     let ic=0
46100     let number$="30"
46200     let cmask$(ic+=1)=number$
46300     let cmask$(ic+=1)=""
46400     let cmask$(ic+=1)=""
46500     let cmask$(ic+=1)=number$
46600     let cmask$(ic+=1)=number$
46700     let cmask$(ic+=1)=number$
46800     let cmask$(ic+=1)=number$
46900     let cmask$(ic+=1)=number$
47000     let cmask$(ic+=1)=number$
47100     let cmask$(ic+=1)=number$
47200     let cmask$(ic+=1)=number$
47300     let cmask$(ic+=1)=number$
47400     let cmask$(ic+=1)=number$
47500     let cmask$(ic+=1)=number$
47600     let cmask$(ic+=1)=number$
47700     let cmask$(ic+=1)=number$
47800     let cmask$(ic+=1)=number$
47900     let cmask$(ic+=1)=number$
48000     let cmask$(ic+=1)=number$
48100     let cmask$(ic+=1)=number$
48200     let cmask$(ic+=1)=number$
48300     let cmask$(ic+=1)=""
48400     let option2$(1)="D = Detail (Pulls amounts from G/L accounts)"
48500     let option2$(2)="T = Total  (Used to print totals or subtotals)"
48600     let option2$(3)="R = Report Heading (Places name of report in heading)"
48700     let option2$(4)="H = Header (Places headings within the F/S)"
48800     let option2$(5)="S = Sub Heading (Places sub heading at top of F/S)"
48900     let option2$(6)="F = Footnote (Used to place footnotes at bottom of F/S)"
49000     let option2$(7)="P = Profit or Loss (Used to place P & L amount on B/S"
49100     let option2$(8)="E = Equity (Used to combine P&L with equity"
49200     let option2$(9)="B = Bank Accounts (Beginning and Ending on Fund Stmt)"
49300     let option2$(10)="C = Cash Flow Pause Indicator (Pauses & asks amounts)"
49400   fnend 
49500 FIXRNP: ! 
49600   reread #fin_stmt,using "Form POS 1,C 5,C 50,C 1,2*N 2,15*N 1,N 3,N 5": rno$ norec L630,eof EO_FIN_STMT_GRID,ioerr L9020
49700 L9020: delete #fin_stmt,key=rno$: 
49800   goto L600
