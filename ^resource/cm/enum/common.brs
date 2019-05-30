
Enum: ! r:Common Enumerations for Collection-Master Add-On
	if ~setupEnum then
		setupEnum=1
		tab$=chr$(9)
		true=yes=1
		false=no=0
		cancel=esc=99
		tab$=chr$(9)
		lf$=chr$(10)
		crlf$=chr$(13)&lf$

		if uprc$(env$("Developer"))="YES" then developer=1
		if uprc$(env$("Debug"))="YES" then debug=1

		! r: MessageBox
		if ~setup_messagebox then
			setup_messagebox=1
			mb_ok=1 : mb_cancel=2 : mb_abort=3 : mb_retry=4 : mb_ignore=5 : mb_yes=6 : mb_no=7 ! Response Enumerations
			mb_stop=16 : mb_question=32 : mb_exclamation=48 : mb_information=64 ! Icon Enumerations
			mb_button1_default=0 : mb_button2_default=256 : mb_button3_default=512 ! Default Button Enumerations
			mb_okonly=0 : mb_okcancel=1 : mb_abortretryignore=2 : mb_yesnocancel=3 : mb_yesno=4 : mb_retrycancel=5 ! Button Set Enumerations
		end if
		! /r
		! r: Internal Backbone Relation Enumerations
		relation_masforw=1 : relation_clerkf=2 : relation_copf=3 : relation_advafile=4
		relation_bankfile=5 : relation_masco=6 : relation_attyfile=7 : relation_secyfile=8
		relation_clientf=9 : relation_employer=10 : relation_collfile=11 : relation_sales=12
		relation_group=13 : relation_trustee=14 : relation_lender=15 : relation_broker=16
		relation_judge=17 : relation_insuranc=18 : relation_titleco=19 : relation_cccs=20
		relation_procserv=21 : relation_hospital=22 : relation_medrec=23 : relation_doctor=24
		relation_mediator=25 : relation_repo=26 : relation_contact=27 : relation_enetwork=28
		! /r
		! r:  Standard FKey Enumerations
		fkey_menu           	= 98
		fkey_exit           	= 93
		fkey_escape         	= 99
		fkey_ae_field_exit 	=107
		fkey_pageup         	= 90
		fkey_pagedown       	= 91
		fkey_tab_change     	= 92
		fkey_click          	=200
		fkey_click_double  	=201
		! /r
	end if
return ! /r