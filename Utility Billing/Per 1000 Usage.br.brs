00010 ! formerly S:\acsUB\Per1000
00020 ! -- Per 1000 Usage Report
12000 ! ______________________________________________________________________
12020   library 'S:\Core\Library': fntop,fnxit, fnacs,fnlbl,fntxt,fntos,fnerror,fnopenprn,fncloseprn,fnd1,fncomboa,fncmdset,fnget_services,fncreg_read,fncreg_write
12040   on error goto ERTN
12060 ! ______________________________________________________________________
12080   dim z$*10,e$(4)*30,g(12)
12100   dim a(7),d(15)
12120   dim range(16),excess(2999,2),cust(16),over(160)
12140   dim resp$(20)*40,text$*40
12160   dim servicename$(10)*20
12180 ! ______________________________________________________________________
14000   fntop(program$)
14020   fnd1(d1)
14040 ! 
16000   fnget_services(mat servicename$)
16060   mat opt$(3)
16080   opt$(1)="Water"
16100   opt$(2)="Gas"
16120   if trim$(servicename$(3))="Lawn Meter" then 
16140     stfn$="wgl"
16160     opt$(3)="Lawn Meter"
16180   else 
16200     stfn$="wge"
16220     opt$(3)="Electric"
16240   end if 
16260 ! 
18000   gosub USAGE_CHART_ASK_RANGE
18020   open #customer:=1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed 
18040 ! on fkey 99 goto DONE
18060 ! on fkey 5 goto DONE
18080   on pageoflow goto PGOF
18100   fnopenprn
18120 goto READ_CUSTOMER
22000 READ_CUSTOMER: ! r:
22020   read #customer,using F_CUSTOMER: z$,mat e$,mat a,mat d,bal,f,mat g eof ENDUBM
22040 F_CUSTOMER: form pos 1,c 10,4*c 30,pos 143,7*pd 2,pos 217,15*pd 5,pos 292,pd 4.2,pd 4,12*pd 4.2
22060   if weg$="W" and wrate<>a(1) and wrate<>0 then 
22080     goto READ_CUSTOMER
22100   else if weg$="E" and wrate<>a(3) and wrate<>0 then 
22120     goto READ_CUSTOMER
22140   else if weg$="L" and wrate<>a(3) and wrate<>0 then 
22160     goto READ_CUSTOMER
22180   else if weg$="G" and wrate<>a(4) and wrate<>0 then 
22200     goto READ_CUSTOMER
22220   else if f><d1 then 
22240     goto READ_CUSTOMER
22260   end if 
22280   gosub USAGE_CHART_ACCUMULATE
22300   if weg$="W" and d(3)>range(16) then 
22320     ov=min(ov+1,160)
22340     over(ov)=d(3)
22360   else if weg$="E" or weg$="L" and d(7)>range(16) then 
22380     ov=min(ov+1,160)
22400     over(ov)=d(7)
22420   else if weg$="G" and d(11)>range(16) then 
22440     ov=min(ov+1,160)
22460     over(ov)=d(11)
22480   end if 
22500 goto READ_CUSTOMER ! /r
24000 ENDUBM: ! r:
24020   close #1: 
24040   gosub USAGE_CHART
24060   gosub OVER_LIST
24080 goto DONE ! /r
26000 DONE: ! r:
26020   close #1: ioerr ignore
26040   fncloseprn
26060 goto XIT ! /r
28000 XIT: let fnxit
28020 IGNORE: continue 
34000 USAGE_CHART_ASK_RANGE: ! r:
34020   fncreg_read('Per 1000 Usage - Rate Code ',weg$, 'W')
34040   fncreg_read('Per 1000 Usage - Service for Analysis ',wrate$, '0') : wrate=val(wrate$)
34060   for rangeItem=1 to 16
34080     fncreg_read('Per 1000 Usage - Range '&str$(rangeItem),tmp$) : range(rangeItem)=val(tmp$)
34100   nex rangeItem
34120   if sum(mat range)=0 then 
34140     let range(1) =    0 : let range(2) = 1000 : let range(3) = 2000
34160     let range(4) = 3000 : let range(5) = 4000 : let range(6) = 5000
34180     let range(7) = 6000 : let range(8) = 7000 : let range(9) = 8000
34200     let range(10)= 9000 : let range(11)=10000 : let range(12)=15000
34220     let range(13)=20000 : let range(14)=30000 : let range(15)=40000
34240     let range(16)=50000
34260   end if
34280 ! /r
42000 MENU1: ! r:
42020   fntos(sn$:="Per1000")
42040   let mylen=22
42060   let mypos=mylen+2
42080   let respc=0
42100   fnlbl(2,1,"Billing Date:" ,mylen,1) : let fnlbl(2,36,"(most recent billing date only)") ! ,31,0)
42120   fntxt(2,mypos,8,0,1,"1")
42140   let resp$(respc+=1)=str$(d1)
42160   let text$="Service for Analysis:"
42180   fnlbl(3,1,text$,mylen,1)
42200   fncomboa(stfn$,3,mypos,mat opt$)
42220   let resp$(respc+=1)=opt$(1)
42260   fnlbl(4,1,"Rate Code:",mylen,1)
42280   fntxt(4,mypos,2,0,1,"30")
42300   let resp$(respc+=1)="0"
42320   let text$="Usage Break Points:"
42340   fnlbl(6,1,text$,mylen,1)
42360   for a = 1 to 16
42380     let resp$(respc+=1) = str$(range(a))
42400   next a
42420   let mypos(1)=mylen+2 : let mypos(2)=mypos(1)+9
42440   let mypos(3)=mypos(2)+9 : let mypos(4)=mypos(3)+9
42460   fntxt(6,mypos(1),7) : let fntxt(6,mypos(2),7)
42480   fntxt(6,mypos(3),7) : let fntxt(6,mypos(4),7)
42500   fntxt(7,mypos(1),7) : let fntxt(7,mypos(2),7)
42520   fntxt(7,mypos(3),7) : let fntxt(7,mypos(4),7)
42540   fntxt(8,mypos(1),7) : let fntxt(8,mypos(2),7)
42560   fntxt(8,mypos(3),7) : let fntxt(8,mypos(4),7)
42580   fntxt(9,mypos(1),7) : let fntxt(9,mypos(2),7)
42600   fntxt(9,mypos(3),7) : let fntxt(9,mypos(4),7)
42620   fncmdset(3)
42640   fnacs(sn$,win,mat resp$,ck)
44000   if ck=5 then goto XIT
44020   let d1=val(resp$(1))
44040   let weg$=resp$(2)(1:1)
44060   let wrate=val(resp$(3))
44080   for a = 1 to 16
44100     let range(a) = val(resp$(a+3))
44120   next a
48000   if weg$<>"W" and weg$<>"E" and weg$<>"G" and weg$<>"L" then ce=3 : goto MENU1
48020   fncreg_write('Per 1000 Usage - Rate Code ',weg$)
48040   fncreg_write('Per 1000 Usage - Service for Analysis ',str$(wrate))
48060   for rangeItem=1 to 16
48080     fncreg_write('Per 1000 Usage - Range '&str$(rangeItem),str$(range(rangeItem)))
48100   nex rangeItem
48120   if weg$="W" then 
48140     let rtype$="Water"
48160   else if weg$="E" then 
48180     let rtype$="Electric"
48200   else if weg$="L" then 
48220     let rtype$="Lawn Meter"
48240   else if weg$="G" then 
48260     let rtype$="Gas"
48280   end if 
48300 return  ! /r
52000 USAGE_CHART_ACCUMULATE: ! r:
52020   for j=1 to 15
52040     if weg$="W" and d(3)=>range(j) and d(3)<range(j+1) then 
52060       cust(j)+=1
52080       goto L860 
52100     else if weg$="E" or weg$="L" and d(7)=>range(j) and d(7)<range(j+1) then 
52120       cust(j)=cust(j)+1
52140       goto L860 
52160     else if weg$="G" and d(11)=>range(j) and d(11)<range(j+1) then 
52180       cust(j)=cust(j)+1
52200       goto L860
52220     end if
52240   next j
52260   cust(16)+=1
52280   let x+=1 
52300   excess(x,1)=val(z$)
52320   if weg$="W" then 
52340     excess(x,2)=d(3) 
52360   else if weg$="E" or weg$="L" then 
52380     excess(x,2)=d(7) 
52400   else if weg$="G" then 
52420     excess(x,2)=d(11)
52440   end if
52460   L860: !
52480 return ! /r
54000 USAGE_CHART: ! r:
54020 ! pr #255: "{\ul                                                                                }"
54040   pr #255: "\qc  {\f181 \fs20 \b "&env$('cnam')&"}"
54060   pr #255: "\qc  {\f181 \fs24 \b "&env$('program_caption')&"}"
54080   pr #255: "\qc  {\f181 \fs16 \b "&date$("Month DD, CCYY")&"}"
54100   pr #255: "\qc  For the Billing Date of "&cnvrt$("PIC(##/##/##)",d1)
54120   if wrate<>0 then 
54140     pr #255: "\qc For "&rtype$&" Rate Code "&str$(wrate)
54160   else 
54180     pr #255: "\qc For "&rtype$
54200   end if
54220   pr #255: "\ql "
54240   pr #255,using "Form POS 7,C 18,C 30": " Usage In","  Total" 
54260   pr #255,using "Form POS 7,C 18,C 30": " Gallons ","Customers"
54280   for j=1 to 16
54300     if j=1 then 
54320       pr #255,using L990: "Under "&ltrm$(cnvrt$("PIC(ZZZZ,ZZ#)",range(2))),cust(j) 
54340       L990: form pos 1,cr 19,x 1,pic(zz,zzz,zz#)
54360     else if range(j)>10 and j<>16 then 
54380       pr #255,using L1000: cnvrt$("PIC(ZZZZ,ZZ#)",range(j))&" - "&ltrm$(cnvrt$("PIC(ZZZZ,ZZ#)",range(j+1)-1)),cust(j) 
54400       L1000: form pos 1,cr 19,x 1,pic(zz,zzz,zz#)
54420     else if j=16 then 
54440       pr #255,using L1000: cnvrt$("PIC(ZZZZ,ZZ#)",range(j))&" or more",cust(j) 
54460     else 
54480       pr #255,using L1010: range(j),cust(j)
54500       L1010: form pos 7,pic(zzzz,zz#),x 6,pic(zz,zzz,zz#)
54520     end if
54540   next j
54560   pr #255: "{\ul                                                                                }"
54580 return ! /r
58000 OVER_LIST: ! r:
58020   if over(1)<>0 then 
58040     pr #255: "Actual usages for customers over "&ltrm$(cnvrt$("PIC(ZZZZ,ZZ#)",range(16)))
58060   end if
58080   for j=1 to 160 step 8
58100     if over(j)>0 then 
58120       pr #255,using "form pos 1,Nz 9,x 1,Nz 9,x 1,Nz 9,x 1,Nz 9,x 1,Nz 9,x 1,Nz 9,x 1,Nz 9,x 1,Nz 9": over(j),over(j+1),over(j+2),over(j+3),over(j+4),over(j+5),over(j+6),over(j+7) 
58140     else 
58160       goto L1110
58180     end if
58200   next j
58220   L1110: !
58240   if over(1)<>0 then 
58260     pr #255: "{\ul                                                                                }"
58280   end if
58300 return ! /r
62000 PGOF: pr #255: newpage : continue 
66000 ! <Updateable Region: ERTN>
66020 ERTN: let fnerror(program$,err,line,act$,"xit")
66040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
66060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
66080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
66100 ERTN_EXEC_ACT: execute act$ : goto ERTN
66120 ! /region
