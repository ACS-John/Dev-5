! ______________________________________________________________________
	library 'S:\Core\Library': fnAcs,fnLbl,fnTxt,fnerror,fnTos,fnxit,fntop,fnpause,fngethandle
! on error goto Ertn
! ______________________________________________________________________
	dim ar(5),ph2$*12,ss2$*11,arta(2),cm$*70
	dim z$*5,a$(5)*30,ph$*12,ss$*11,dd(10),sc(10)
	dim app(20),ma(20),ap2(20),ma2(20),ca(10)
! ______________________________________________________________________
! 
	fntop(program$,"CLmstr to Support")
	dim system_id$(20)*2
	system_id$(01)='GL'
	system_id$(02)='AR'
	system_id$(03)='AP'
	system_id$(04)='UB'
	system_id$(05)='PB'
	system_id$(06)='PT'
	system_id$(07)='HA'
	system_id$(08)='FA'
	system_id$(09)='TM'
	system_id$(10)='CR'
	system_id$(11)='HH'
	system_id$(12)='IV' ! Invoicing
	system_id$(13)='IN' ! Inventory
	system_id$(14)='PR'
	system_id$(15)='PO'
	system_id$(16)='MC'
	system_id$(17)='??' ! 2010-2011 Payroll Changes ???
	system_id$(18)='CL'
	system_id$(19)='CO'
	system_id$(20)='??' ! Printing
	date_start=20110701
	date_end=20110731
	open #1: "Name=S:\Core\Data\acsllc\CLmstr.H[cno],KFName=S:\Core\Data\acsllc\CLIndex.H[cno],Shr",internal,outIn,keyed 
	open #11: "Name=S:\Core\Data\acsllc\CLmstr.H[cno],KFName=S:\Core\Data\acsllc\CLIndx2.H[cno],Shr",internal,outIn,keyed 
FORM_CLMSTR: form pos 1,c 5,5*c 30,c 12,c 11,n 9,n 2,10*pd 3,10*n 1,10*pd 3,c 12,c 11,2*pd 5.2,pd 4.3,2*n 1,2*pd 3,c 70,20*n 1,20*pd 3.2,20*n 1,20*pd 3.2
	open #h_support:=2: "Name=S:\Core\Data\acsllc\Support.h[cno],Version=2,KFName=S:\Core\Data\acsllc\Support-Idx.h[cno],Shr",internal,outIn,keyed 
FORM_SUPPORT: form pos 1,n 6,n 2,c 2,n 8,c 2,n 8,n 10.2,4*c 50
	z$=lpad$(str$(ano),5)
	do 
		read #1,using FORM_CLMSTR: z$,mat a$,ph$,ss$,pno,mye,mat dd,mat sc,mat ca,ph2$,ss2$,mat ar,mat arta,cm$,mat app,mat ma,mat ap2,mat ma2 eof EO_CLMSTR
		client_id=val(z$)
! IF client_id=1500 then pr client_id : pause
		if sum(ma)>0 then 
			which_app=fn_first_item_gtr_than_one(mat ma)
			if which_app>0 then 
				ma(which_app)=ma(which_app)-1
				ma(19)=1
				app(19)=1
			end if  ! which_app>0
		end if  ! sum(app)>0
		for app_item=1 to udim(mat app)
			if app(app_item) and ma(app_item)>.01 then 
				support_key$=lpad$(str$(client_id),6)&lpad$(str$(app_item),2)
				rewrite #h_support,using FORM_SUPPORT,key=support_key$: client_id,app_item,system_id$(app_item),date_start,'Mo',date_end,ma(app_item),'','','','' nokey WR_SUPPORT
			end if  ! app(app_item)
		next app_item
		mat ma=(0)
		rewrite #1,using FORM_CLMSTR: z$,mat a$,ph$,ss$,pno,mye,mat dd,mat sc,mat ca,ph2$,ss2$,mat ar,mat arta,cm$,mat app,mat ma,mat ap2,mat ma2
	loop 
WR_SUPPORT: ! 
	write #h_support,using FORM_SUPPORT: client_id,app_item,system_id$(app_item),date_start,'Mo',date_end,ma(app_item),'','','',''
	continue  ! WR_SUPPORT
EO_CLMSTR: ! 
	end 
	def fn_first_item_gtr_than_one(mat fig_array)
		fig_found=0
		for fig_item=1 to udim(mat fig_array)
			if fig_array(fig_item)>1 then fig_found=fig_item : goto FIG_XIT
		next fig_item
FIG_XIT: ! 
		fn_first_item_gtr_than_one=fig_found
	fnend  ! fn_first_item_gtr_than_one
