10000 ! Replace S:\acsGL\actrans_change_glnumber
10200 ! ______________________________________________________________________
10400   library 'S:\Core\Library': fntop,fnxit,fnacs,fnlbl,fntxt,fngethandle,fntos,fnerror,fncno,fnmsgbox,fncmdset,fnchk,fncd,fnactpd,fnopenprn,fncloseprn
10600   on error goto ERTN
10800 ! ______________________________________________________________________
11000   dim cnam$*40,cap$*128,resp$(100)*60
11200   dim balance_current_year_month(13),balance_prior_year_month(13),rf(6)
11400   dim actrans_key$*20
11600 ! ______________________________________________________________________
11800   fncno(cno,cnam$)
12000   fntop(program$, cap$="Change GL Numbers in ACTrans")
12200 ! 
13000   let gln_from$=' 12   101  0' : let gln_to$='  1   101  0'
13800   if fn_screen_1(gln_from$,gln_to$)=5 then goto XIT
14000   fn_report(cap$)
14200   fn_report(date$('mm/dd/ccyy'))
14400   fn_report('')
14600   open #h_actrans:=fngethandle: "Name="&env$('Q')&"\GLmstr\AcTrans.H"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\AcTrIdx.H"&str$(cno)&",Shr",internal,outin,keyed 
14800 F_ACTRANS: form pos 1,c 12,n 6,pd 6.2,n 2,pos 71,n 2
15000   restore #h_actrans,key>=rpad$(gln_from$,kln(h_actrans)): 
15200   do 
15400     read #h_actrans,using F_ACTRANS: gl$
15600     if gl$>gln_from$ then goto EO_ACTRANS
15800     if gl$=gln_from$ then 
16000       fn_report('rec '&str$(rec(h_actrans)))
16200       let gl$=gln_to$
16400       rewrite #h_actrans,using F_ACTRANS: gl$
16600     end if  ! gln_period_did_change>0
16800   loop 
17000 EO_ACTRANS: ! 
17200   fncloseprn : let report_open=0
17400 XIT: let fnxit
17600 ! ______________________________________________________________________
17800   def fn_screen_1(&gln_from$,&gln_to$)
18000     fntos(sn$="FixGLN")
18200     let mylen=22
18400     let mypos=mylen+2
18600     let respc=0 : let myline=0
18800     fnlbl(myline+=1,1,"Change GL Number From:",mylen,1)
19000     fntxt(myline,mypos,12,0,1)
19200     let resp$(respc+=1)=gln_from$
19400     fnlbl(myline+=1,1,"To:",mylen,1)
19600     fntxt(myline,mypos,12,0,1)
19800     let resp$(respc+=1)=gln_to$
20200     fncmdset(2)
20400     fnacs(sn$,0,mat resp$,ck)
20600     if ck<>5 then 
20800       let gln_from$=lpad$(resp$(1),12)
21000       let gln_to$=lpad$(resp$(2),12)
21200     end if  ! ck<>5 then
21400     fn_screen_1=ck
21600   fnend  ! fn_screen_1
21800   def fn_report(line$*256)
22000     if ~report_open then 
22200       let report_open=1
22400       fnopenprn
22600     end if  ! ~report_open
22800     pr #255: line$ ! if gl$='  6   101  0' then pr #255: line$
23000   fnend 
51670 ! ______________________________________________________________________
51680 ! <Updateable Region: ERTN>
51690 ERTN: let fnerror(program$,err,line,act$,"xit")
51700   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
51710   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
51720   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
51730 ERTN_EXEC_ACT: execute act$ : goto ERTN
51740 ! /region
