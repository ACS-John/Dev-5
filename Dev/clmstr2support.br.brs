00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fnerror,fntos,fncno,fnxit,fntop,fnpause,fngethandle
00050 ! on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128
00090   dim cnam$*40
00120   dim ar(5),ph2$*12,ss2$*11,arta(2),cm$*70,cnam$*40
00130   dim z$*5,a$(5)*30,ph$*12,ss$*11,dd(10),sc(10)
00210   dim app(20),ma(20),ap2(20),ma2(20),ca(10)
20000 ! ______________________________________________________________________
20200   fncno(cno,cnam$)
20400 ! 
20600   fntop(program$(4:len(program$)-3),cap$=env$('Q')&"\CLmstr to Support")
20800   dim system_id$(20)*2
21000   system_id$(01)='GL'
21200   system_id$(02)='AR'
21400   system_id$(03)='AP'
21600   system_id$(04)='UB'
21800   system_id$(05)='PB'
22000   system_id$(06)='PT'
22200   system_id$(07)='HA'
22400   system_id$(08)='FA'
22600   system_id$(09)='TM'
22800   system_id$(10)='CR'
23000   system_id$(11)='HH'
23200   system_id$(12)='IV' ! Invoicing
23400   system_id$(13)='IN' ! Inventory
23600   system_id$(14)='PR'
23800   system_id$(15)='PO'
24000   system_id$(16)='MC'
24200   system_id$(17)='??' ! 2010-2011 Payroll Changes ???
24400   system_id$(18)='CL'
24600   system_id$(19)='CO'
24800   system_id$(20)='??' ! Printing
25000   date_start=20110701
25200   date_end=20110731
25400   open #1: "Name="&env$('Q')&"\TMmstr\CLmstr.H"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\CLIndex.H"&str$(cno)&",Shr",internal,outin,keyed 
25600   open #11: "Name="&env$('Q')&"\TMmstr\CLmstr.H"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\CLIndx2.H"&str$(cno)&",Shr",internal,outin,keyed 
25800 FORM_CLMSTR: form pos 1,c 5,5*c 30,c 12,c 11,n 9,n 2,10*pd 3,10*n 1,10*pd 3,c 12,c 11,2*pd 5.2,pd 4.3,2*n 1,2*pd 3,c 70,20*n 1,20*pd 3.2,20*n 1,20*pd 3.2
26000   open #h_support:=2: "Name="&env$('Q')&"\TMmstr\Support.h"&str$(cno)&",Version=2,KFName="&env$('Q')&"\TMmstr\Support-Idx.h"&str$(cno)&",Shr",internal,outin,keyed 
26200 FORM_SUPPORT: form pos 1,n 6,n 2,c 2,n 8,c 2,n 8,n 10.2,4*c 50
26400   let z$=lpad$(str$(ano),5)
26600   do 
26800     read #1,using FORM_CLMSTR: z$,mat a$,ph$,ss$,pno,mye,mat dd,mat sc,mat ca,ph2$,ss2$,mat ar,mat arta,cm$,mat app,mat ma,mat ap2,mat ma2 eof EO_CLMSTR
27000     client_id=val(z$)
27200 ! IF client_id=1500 then pr client_id : pause
27400     if sum(ma)>0 then 
27600       let which_app=fn_first_item_gtr_than_one(mat ma)
27800       if which_app>0 then 
28000         ma(which_app)=ma(which_app)-1
28200         ma(19)=1
28400         app(19)=1
28600       end if  ! which_app>0
28800     end if  ! sum(app)>0
29000     for app_item=1 to udim(mat app)
29200       if app(app_item) and ma(app_item)>.01 then 
29400         support_key$=lpad$(str$(client_id),6)&lpad$(str$(app_item),2)
29600         rewrite #h_support,using FORM_SUPPORT,key=support_key$: client_id,app_item,system_id$(app_item),date_start,'Mo',date_end,ma(app_item),'','','','' nokey WR_SUPPORT
29800       end if  ! app(app_item)
30000     next app_item
30200     mat ma=(0)
30400     rewrite #1,using FORM_CLMSTR: z$,mat a$,ph$,ss$,pno,mye,mat dd,mat sc,mat ca,ph2$,ss2$,mat ar,mat arta,cm$,mat app,mat ma,mat ap2,mat ma2
30600   loop 
30800 WR_SUPPORT: ! 
31000   write #h_support,using FORM_SUPPORT: client_id,app_item,system_id$(app_item),date_start,'Mo',date_end,ma(app_item),'','','',''
31200   continue  ! WR_SUPPORT
31400 EO_CLMSTR: ! 
31600   end 
31800   def fn_first_item_gtr_than_one(mat fig_array)
32000     let fig_found=0
32200     for fig_item=1 to udim(mat fig_array)
32400       if fig_array(fig_item)>1 then let fig_found=fig_item : goto FIG_XIT
32600     next fig_item
32800 FIG_XIT: ! 
33000     fn_first_item_gtr_than_one=fig_found
33200   fnend  ! fn_first_item_gtr_than_one
