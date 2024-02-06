autoLibrary

fn_messageBox('test')
end

def fn_messageBox(text$*2048; type,title$*200,___,hTextCols)
	autoLibrary
	Orig_Mb_Fkey=fkey
	! mbx_name$*20 - if included than a checkbox for "[ ] don't ask me again next time" will be added to the message
	if ~Mbx_Setup then
		Mbx_Setup=1
		dim dontAskAgainText$*40
		dim winData$(1)*128,Mbx_Win$*2048,Mbx_Name$*40,Mbx_Button_Fkey_List$*256
		dim fileOs$(1)*256
		dim Mbx_File_V$(1)*256
		dim Mbx_Win_Field$(1)*40,Mbx_Win_Text$(1)*40
		!   dontAskAgainText$='Remember my answer next time.'
		Mbx_Wait_Time=15*60 ! if jbowman then mbx_wait_time=3 else mbx_wait_time=15*60
	end if  ! ~mbx_setup
	if lwrc$(env$('Messagebox_Always_Ask'))='yes' then Mbx_Always_Ask=1 else Mbx_Always_Ask=0 ! note this line - this line forces the questions to be ask even if they have choosen not to remember prior answer.
	text$=srep$(text$,Crlf$,Lf$)
	text$=srep$(text$,Cr$,Lf$)
	text$=srep$(text$,'\n',Lf$)
	fn_Size(text$,Mb3_Text_Rows,Mb3_Cols)
	Mb3_Text_Rows=Max(Mb3_Text_Rows,5)
	Mb3_Parent_Rows=Mb3_Text_Rows+2
	Mb3_Cols=Min(Mb3_Cols,80)
	Mbx_Button_Default=fn_buttonDefault(type)
	Mbx_Icon$=fn_Icon$(type)
	Mbx_Button_Count=fn_Button(type,Mbx_Button_Text$,mat Mbx_Button_Return)
	if Mbx_Name$<>'' then
		Mb3_Parent_Rows+=1 ! add line for dont ask again text and a blank line
		if udim(mat Mbx_Button_Return)=1 then
			dontAskAgainText$='do not notify me again.'
		else
			dontAskAgainText$='Remember my answer next time.'
		end if
		Mb3_Cols=Max(Mb3_Cols,Len(dontAskAgainText$)+10)
		Mbx_Dont_Ask_Row=Mb3_Parent_Rows-1 ! mbx_dont_ask_row is the row to print the dont ask again text on
	end if  ! mbx_name$<>''
	Mbx_Button_Fkey_List$=''
	mat Mbx_Button_Fkey(Mbx_Button_Count)
	for Mbx_Button_Fkey_Item=1 to Mbx_Button_Count
		Mbx_Button_Fkey(Mbx_Button_Fkey_Item)=2010+Mbx_Button_Fkey_Item
		Mbx_Button_Fkey_List$(Inf:0)=str$(Mbx_Button_Fkey(Mbx_Button_Fkey_Item))&';'
	next Mbx_Button_Fkey_Item
	Mbx_Button_Fkey_List$=Mbx_Button_Fkey_List$(1:Len(Mbx_Button_Fkey_List$)-1) ! remove the last ;

	if type<>0 then print 'type<>0 - something must be wrong' : PAUSE

	if Mbx_Name$<>'' then
		Mbx_Name$='mbx_'&trim$(Mbx_Name$)&'_'&str$(Mb3_Parent_Rows)&'x'&str$(Mb3_Cols)
		Mbx_Win$='name='&Mbx_Name$&','
		! Fnregunum_Get(Mbx_Name$)
		fnureg_read('messageBox.'&Mbx_Name$,tmp) : Mbx_Checked=tmp
	else
		Mbx_Checked=0
	end if  ! mbx_name$<>''   /   else
	Mbx_File_Count=fn_getFileList(Mbx_H_Text,text$,Mbx_H_Cols,mat fileOs$,mat Mbx_File_V$)
	if Mbx_Always_Ask Or Mbx_Checked<=9999 then
		mat Mbx_Win_Field$(1)=('')
		mat Mbx_Win_Text$(1)=('')
		Mbx_Win$=''
		Mbx_Timeout=0
!   if mbx_checked>9999 then dontAskAgainText$='^'&dontAskAgainText$
		Mbx_Win$(Inf:0)='Caption='&srep$(title$,',','.')
		Mbx_Win$(Inf:0)=',rows='&str$(Mb3_Parent_Rows)
		Mbx_Win$(Inf:0)=',cols='&str$(Mb3_Cols)
		Mbx_Win$(Inf:0)=',center'
		Mbx_Win$(Inf:0)=',button.text='&Mbx_Button_Text$ ! &';(f)Copy'
		if Mbx_File_Count>0 then
			Mbx_Win$(Inf:0)=';(f)File'
			if Mbx_File_Count>1 then Mbx_Win$(Inf:0)='s'
!     mbx_button_default+=1 ! because File will be the first button, so skip it.
		end if  ! mbx_file_count>0
		Mbx_Win$(Inf:0)=',button.fkey='&Mbx_Button_Fkey_List$ ! &';33'
		if Mbx_File_Count>0 then
			Mbx_Win$(Inf:0)=';33'
		end if  ! mbx_file_count>0
		Mbx_Win$(Inf:0)=',parent=none'
		if ~Developer then Mbx_Win$(Inf:0)=',modal,Nomaximize'
		open #Mbx_H_Win=fnH: fn_openParent$(Mbx_Win$,mat winData$,1,1),d,o
		fn_Generate_Buttons_For_Window(mat winData$,Mbx_H_Win, 1,mat Mbx_Win_Field$,mat Mbx_Win_Text$)
		fn_arrayReverse$(mat Mbx_Win_Field$)
		fn_arrayReverse$(mat Mbx_Win_Text$)
		if Exists('Icons\'&Mbx_Icon$&'.png') then print #Mbx_H_Win,FIELDS '1,1,P 4/8,[W]': 'Icons\'&Mbx_Icon$&'.png:isotropic'
		Mbx_H_Cols=val(winData$(Pw_Cols))
		Mbx_H_Text_Rows=Mb3_Text_Rows
		hTextCols=Mbx_H_Cols-10
		open #Mbx_H_Text=fnH: 'Modal,Parent='&str$(Mbx_H_Win)&',Border=None,SRow=1,SCol=10,Rows='&str$(Mbx_H_Text_Rows)&',Cols='&str$(hTextCols),d,o
! if jbowman then print 'text$='&text$ : pause
		print #Mbx_H_Text,FIELDS '1,1,'&fn_textSize$(hTextCols,Mbx_H_Text_Rows)&',[T]S,49': text$
		if Mbx_Name$<>'' then
			fnAddOneC(mat Mbx_Win_Field$,str$(Mbx_Dont_Ask_Row)&',10,Check '&str$(Len(dontAskAgainText$)+1)&',[W]ae,2020')
			fnAddOneC(mat Mbx_Win_Text$,Rpt$('^',Min(1,Mbx_Checked-10000))&dontAskAgainText$)
			Mbx_Win_Which_Don_Ask=udim(mat Mbx_Win_Text$)
			if Mbx_Checked-10000>0 then Mbx_Button_Default=Srch(mat Mbx_Button_Fkey,Mbx_Checked-10000)
		end if  ! mbx_name$<>''
		for Mbx_Win_Field_Item=1 to udim(mat Mbx_Win_Field$)
			Mbx_Win_Field$(Mbx_Win_Field_Item)=srep$(Mbx_Win_Field$(Mbx_Win_Field_Item),'[Button]','[Button]ae')
		next Mbx_Win_Field_Item
		Curfld(Mbx_Button_Default)
		do
			Mb_X_Click=0
			RINPUT #Mbx_H_Win,SELECT mat Mbx_Win_Field$,ATTR '[A]',WAIT=Mbx_Wait_Time: mat Mbx_Win_Text$ TIMEOUT IGNORE
			if fkey=93 Or fkey=99 then
				goSub MBX_DEFAULT_FKEY
				Mb_X_Click=Mb_X
			else if fkey=101 then
				Mbx_Timeout=1
				goSub MBX_DEFAULT_FKEY
			else if fkey=2020 then
				if Mbx_Win_Text$(Mbx_Win_Which_Don_Ask)(1:1)='^' then
					Mbx_Win_Text$(Mbx_Win_Which_Don_Ask)(1:1)=''
				else
					Mbx_Win_Text$(Mbx_Win_Which_Don_Ask)(0:0)='^'
				end if  ! mbx_win_text$(mbx_win_which_don_ask)(1:1)='^'   /   else
			else if fkey=33 and Mbx_File_Count>0 then ! Alt+F
				fn_fileMenu(title$,Mbx_File_Count,hTextCols,mat fileOs$,mat Mbx_File_V$,Mbx_Wait_Time)
			else if fkey=46 then ! Alt+C
				Setenv('ClipBoard',srep$(text$,Lf$,Crlf$))
			else if fkey=99 and Mbx_Button_Count=1 then
				fkey(Mbx_Button_Fkey(1))
			end if  ! fkey=...
		loop until srch(mat Mbx_Button_Fkey,fkey)>0
		if Mbx_Name$<>'' and fkey<>101 then
			if Mbx_Win_Text$(Mbx_Win_Which_Don_Ask)(1:1)='^' then Mbx_Checked=1 else Mbx_Checked=0
			if Mbx_Checked then
				! Fnregunum_Put(Mbx_Name$,10000+fkey)
				fnureg_write('messageBox.'&Mbx_Name$,str$(10000+fkey))
			else
				! Fnregunum_Put(Mbx_Name$,0)
				fnureg_write('messageBox.'&Mbx_Name$,'0')
			end if  ! mbx_checked   /   else
		end if  ! mbx_name$=''
		close #Mbx_H_Win:
	else
!   print 'returning with fkey '&str$(mbx_checked-10000)&' from memory'
		fkey(Mbx_Checked-10000)
!   pause
	end if  ! mbx_always_ask or mbx_checked<=9999   /   else
	goto MBX_XIT
MBX_DEFAULT_FKEY: !
	if Curfld<=Mbx_Button_Count and Srch(mat Mbx_Button_Fkey,Mbx_Button_Fkey(Curfld))>0 then
		fkey(Mbx_Button_Fkey(Curfld))
	else
		fkey(Mbx_Button_Fkey(Mbx_Button_Default))
	end if  ! curfld<=mbx_button_count and srch(mat mbx_button_fkey,mbx_button_fkey(curfld))>0   /   else
	RETURN  ! MBX_DEFAULT_FKEY
MBX_XIT: !
	if Mb_X_Click then
		fn_messageBox=Mb_X_Click
	else
		fn_messageBox=Mbx_Button_Return(Srch(mat Mbx_Button_Fkey,fkey))
	end if
	if Mbx_Timeout then
		fkey(101)
	end if
	if fkey<>93 and fkey<>101 then
		fkey(Orig_Mb_Fkey)
	end if
	Orig_Mb_Fkey=0
fnend
def fnarrayreverse$(mat in$,mat out$)
	in_udim=udim(in$)
	mat out$(in_udim)
	for in_item=1 to in_udim
		out$(in_udim-in_item+1)=in$(in_item)
	next in_item
fnend
def fn_fileMenu(Mbxfm_Title$*80,fileCount, _
	Mbxfm_H_Text_Cols,mat fileName$, _
	mat Mbxfm_File_V$,Mbxfm_Wait_Time; ___, _
	actionWin$*256,filePath$*256,fileNameBase$*128,fileNameExt$*128, _
	selection$*256,s$*1,actionTab$*256 _
	)

	dim actionWinData$(1)*128
	dim hTab(2)
	
	if fileCount>1 then s$='s'
	actionWin$(Inf:0)='Caption='&Mbxfm_Title$&' - File'&s$
	actionWin$(Inf:0)=',center'
	actionWin$(Inf:0)=',rows='&str$(fileCount+3)
	actionWin$(Inf:0)=',cols='&str$(Mbxfm_Win_Action_Cols:=Max(40,Mbxfm_H_Text_Cols)+2)
	actionWin$(Inf:0)=',button.text=close;(f)Copy;(f)open;(f)open Folder'
	actionWin$(Inf:0)=',button.fkey=99;2001;2002;2003'
	actionWin$(Inf:0)=',parent=none'
	if ~Developer then actionWin$(Inf:0)=',modal,Nomaximize'
	open #Mbxfm_H_Win=fnH: fn_openParent$(actionWin$,mat actionWinData$,1),d,o

	actionTab$='SRow=2,SCol=2,Rows='&str$(fileCount+0)&',Cols='&str$(Mbxfm_Win_Action_Cols-2)&',Parent='&str$(Mbxfm_H_Win)
	fn_Generate_Buttons_For_Window(mat actionWinData$,Mbxfm_H_Win)
	open #hTab(1)=fnH: fn_openParent$(actionTab$&',Tab=OS Path'&s$,mat Mbxfm_Tab_Data$,1),d,o
	open #hTab(2)=fnH: fn_openParent$(actionTab$&',Tab=Virtual Path'&s$,mat Mbxfm_Tab_Data$,1),d,o
	dim fileField$(0)*20
	mat fileField$(fileCount)
	for Mbxfm_File_Item=1 to fileCount
		fileField$(Mbxfm_File_Item)=str$(Mbxfm_File_Item)&',1,40/C '&actionWinData$(Pw_Cols)&',[M]'
	next Mbxfm_File_Item
	! Mbxfm_Curtab=Fnregunum_Get('mbxfm_CurTab')
	fnureg_read('messageBox.CurTab',tmp$) : Mbxfm_Curtab=val(tmp$)
	if Mbxfm_Curtab<=0 then Mbxfm_Curtab=2
	do
		Curfld(Mbxfm_Curfld)
		if Mbxfm_Curtab=1 then
			RINPUT #hTab(Mbxfm_Curtab),SELECT mat fileField$, ATTR '[L]',WAIT=Mbxfm_Wait_Time: mat fileName$ TIMEOUT IGNORE
			selection$=fileName$(Curfld)
		else ! if mbxfm_curtab=2 then
			RINPUT #hTab(Mbxfm_Curtab),SELECT mat fileField$, ATTR '[L]',WAIT=Mbxfm_Wait_Time: mat Mbxfm_File_V$ TIMEOUT IGNORE
			selection$=Mbxfm_File_V$(Curfld)
		end if  ! mbxfm_curtab=1   /   else
		Mbxfm_Curfld=Curfld
		if fkey=92 then
			if Mbxfm_Curtab=1 then Mbxfm_Curtab=2 else Mbxfm_Curtab=1
		else if fkey=101 then
			fkey(99)
		else if fkey=2001 then
			Setenv('ClipBoard',selection$)
		else if fkey=2002 then
			exe 'Sy -c -w -@ '&efExe$&' "'&fileName$(Curfld)&'"'&efSwitches$
		else if fkey=2003 then
			fnGetPp(fileName$(Curfld),filePath$,fileNameBase$,fileNameExt$)
			EXECUTE "System -c -w explorer /select,"&srep$(fileName$(Curfld),"@::","")&'"'
		end if  ! fkey=92   /   101   /   2001   /   2002   /   2003
	loop until fkey=99 Or fkey=93
	! Fnregunum_Put('mbxfm_CurTab',Mbxfm_Curtab)
	fnureg_write('messageBox.CurTab','0')
	close #Mbxfm_H_Win:
fnend  ! fn_fileMenu
def fn_getFileList(Mbxht_H_Parent,Mbxht_Text$*2048,Mbxht_Cols,mat Mbxht_File_Os$,mat Mbxht_File_V$) ! mbxht_
	dim Mbxht_Line$(1)*1024
	Str2mat(Mbxht_Text$,mat Mbxht_Line$) ! No Paramater needed here LF of all types
	mat Mbxht_File_Os$(0)
	mat Mbxht_File_V$(0)
	for Mbxht_Line_Item=1 to udim(mat Mbxht_Line$)
		if Mbxht_Line$(Mbxht_Line_Item)(1:2)='\\' Or Mbxht_Line$(Mbxht_Line_Item)(2:2)=':' then ! the line specifies a file
			Os_Filename$(Mbxht_Line$(Mbxht_Line_Item)) error L30260 ! Check for "Bad File Names"
			fnAddOneC(mat Mbxht_File_V$,Mbxht_Line$(Mbxht_Line_Item),0,1)
			fnAddOneC(mat Mbxht_File_Os$,fn_Client_Os_Path$(Mbxht_Line$(Mbxht_Line_Item)),0,1) ! fn_Os_Filename$ - fn_Client_OS_Path does this!
			L30260: !
		end if  ! mbxht_line$(mbxht_line_item)(1:2)='\\' or mbxht_line$(mbxht_line_item)(2:2)=':'
	next Mbxht_Line_Item
	fn_getFileList=udim(mat Mbxht_File_Os$)
fnend
def fn_textSize$(Mts_Cols,Mts_Rows)
	Mts_Cols_Original=Mts_Cols
	Mts_Rows_Original=Mts_Rows
	fn_textSize$=str$(hTextCols*Mbx_H_Text_Rows)&"/C" ! str$(mts_cols_original*mts_rows_original)&'/Cl '&str$(mts_cols*mts_rows)
fnend
def fn_Size(&message$,&maxRows,&cols; ___,lineItem)
	dim mbxsLine$(0)*1024
	Str2mat(message$,mat mbxsLine$) ! ,LF$ is not needed because STR2MAT checks all types or LF
	maxRows=udim(mat mbxsLine$)
	cols=0
	for lineItem=1 to maxRows
		cols=Max(cols,Len(mbxsLine$(lineItem)))
		cols+=fnChrCount(mbxsLine$(lineItem),Tab$)*4
	next lineItem
	if cols>140 then
		cols=Ceil(cols*.78)
	else if cols>90 then
		cols=Ceil(cols*.82)
	else if cols>60 then
		cols=Ceil(cols*.86)
	else if cols>30 then
		cols=Ceil(cols*.90)
	! else
	!   cols=ceil(cols*1.0)
	end if
	cols=Max(cols,10)
	cols+=13
fnend
def fn_buttonDefault(&type)
	if type>=Mb_Button3_Default then
		type-=Mb_Button3_Default
		Mbx_Button_Default=3
	else if type>=Mb_Button2_Default then
		type-=Mb_Button2_Default
		Mbx_Button_Default=2
	else
		Mbx_Button_Default=1
	end if  ! type
	fn_buttonDefault=Mbx_Button_Default
fnend  ! fn_buttonDefault
def fn_Icon$(&type)
	if type>=Mb_Information then
		type-=Mb_Information
		Mbx_Icon$='information'
	else if type>=Mb_Exclamation then
		type-=Mb_Exclamation
		Mbx_Icon$='exclamation'
	else if type>=Mb_Question then
		type-=Mb_Question
		Mbx_Icon$='question'
	else if type>=Mb_Stop then
		type-=Mb_Stop
		Mbx_Icon$='stop'
	else
!   if developer or debug then print bell;'no icon selected for messagegox - default of "information" will be used.'
!   if developer then pause
		Mbx_Icon$='information'
	end if
	fn_Icon$=Mbx_Icon$
fnend  ! fn_icon$
def fn_Button(&Mbxb_Type,&Mbxb_Button_Text$,mat Mbxb_Button_Return) ! mbxb_
	mat Mbxb_Button_Return(1) : Mbxb_Button_Text$="?"&str$(Mbxb_Type): Mbxb_Button_Return(1)=Mb_Ok
	Mbxb_Button_Text$=''
	if Mbxb_Type>=Mb_Retrycancel then
		Mbxb_Type-=Mb_Retrycancel : Mbxb_Button_Text$='Retry;Cancel'
		mat Mbxb_Button_Return(2) : Mbxb_Button_Return(1)=Mb_Retry
		Mbxb_Button_Return(2)=Mb_Cancel : Mb_X=Mb_Cancel
	else if Mbxb_Type>=Mb_Yesno then
		Mbxb_Type-=Mb_Yesno
		Mbxb_Button_Text$='Yes;No'
		mat Mbxb_Button_Return(2) : Mbxb_Button_Return(1)=Mb_Yes
		Mbxb_Button_Return(2)=Mb_No : Mb_X=Mb_No
	else if Mbxb_Type>=Mb_Yesnocancel then
		Mbxb_Type-=Mb_Yesnocancel
		Mbxb_Button_Text$='Yes;No;Cancel'
		mat Mbxb_Button_Return(3) : Mbxb_Button_Return(1)=Mb_Yes
		Mbxb_Button_Return(2)=Mb_No : Mbxb_Button_Return(3)=Mb_Cancel
		Mb_X=Mb_Cancel
	else if Mbxb_Type>=Mb_Abortretryignore then
		Mbxb_Type-=Mb_Abortretryignore
		Mbxb_Button_Text$='Abort;Retry;Ignore'
		mat Mbxb_Button_Return(3) : Mbxb_Button_Return(1)=Mb_Abort
		Mbxb_Button_Return(2)=Mb_Retry : Mbxb_Button_Return(3)=Mb_Ignore
		Mb_X=Mb_Abort
	else if Mbxb_Type>=Mb_Okcancel then
		Mbxb_Type-=Mb_Okcancel
		Mbxb_Button_Text$='OK;Cancel'
		mat Mbxb_Button_Return(2) : Mbxb_Button_Return(1)=Mb_Ok
		Mbxb_Button_Return(2)=Mb_Cancel : Mb_X=Mb_Cancel
	else
		mat Mbxb_Button_Return(1) : Mbxb_Button_Return(1)=Mb_Ok
		Mbxb_Type-=Mb_Okonly
		Mbxb_Button_Text$='OK'
		Mb_X=Mb_Ok
	end if
	fn_Button=udim(mat Mbxb_Button_Return)
fnend

def fn_Generate_Buttons_For_Window(mat Gbw_Win_Part$,Gbw_H_Window; Gbw_Noadd_Keys,mat Gbw_Win_Button_Field$,mat Gbw_Win_Button_Text$) ! gbw_
	! (f) is for Features - the top row of buttons is for program features, the bottom for of buttons is for navigation.
	dim Gbw_Field$(1)*40
	dim Gbw_Btn_Text$(1)*80,Gbw_Btn_Tmp_Text$*80,Gbw_Btn_Fkey$(1)
	if udim(mat Gbw_Win_Button_Field$)>0 then  Gbw_Win_Button_Pop=1 else  Gbw_Win_Button_Pop=0
	if Gbw_Win_Button_Pop then
		mat Gbw_Win_Button_Field$(0)
		mat Gbw_Win_Button_Text$(0)
	end if  ! gbw_win_button_pop
	 Gbw_Height_Mod=fn_windowButtonRows(mat Gbw_Win_Part$)
	 Gbw_Loop_Start=1
	 Gbw_Loop_End=Gbw_Height_Mod
	if Gbw_Height_Mod=1 and pos(Gbw_Win_Part$(Pw_Button_Text),'(f)')<=0 then  Gbw_Loop_Start=2 :  Gbw_Loop_End=2
	for Mwb_Button_Row=Gbw_Loop_Start to Gbw_Loop_End

		 Buttons_Row=val(Gbw_Win_Part$(Pw_Rows))-2+Mwb_Button_Row

		 Str2mat(Gbw_Win_Part$(Pw_Button_Text),mat B_Lab$,';')
		 Str2mat(Gbw_Win_Part$(Pw_Button_Fkey),mat B_Key$,';')
		 Tot_Keys=udim(B_Key$)
		mat B_Lab$(Tot_Keys)
		mat B_Key(Tot_Keys)
		for Ncount=1 to Tot_Keys ! build mat B_Key from mat B_Key$
			 B_Key(Ncount)=val(B_Key$(Ncount)) CONV IGNORE
		next Ncount
		 M_Row$=str$(Buttons_Row)
		 Status_String$=B_String$=""
		print #Gbw_H_Window,FIELDS M_Row$&",1,C "&Gbw_Win_Part$(Pw_Cols)&",[W]": "" error IGNORE ! Delete Button Row
		mat Gbw_Btn_Text$(0)
		mat Gbw_Btn_Fkey$(0)
		for Dev_J=udim(mat B_Lab$) to 1, STEP -1
			 Gbw_Btn_Tmp_Text$=srep$(srep$(B_Lab$(Dev_J),'(f)',''),'(n)','')
			if (Mwb_Button_Row=1 and pos(B_Lab$(Dev_J),'(f)')>0) Or (Mwb_Button_Row=2 and pos(B_Lab$(Dev_J),'(f)')<=0) then
				if ~Gbw_Noadd_Keys then
!         auto add fkey to label - top
					if B_Key(Dev_J)=99 and pos(lwrc$(Gbw_Btn_Tmp_Text$),"esc")<=0 then
						 Gbw_Btn_Tmp_Text$="[Esc] "&trim$(Gbw_Btn_Tmp_Text$)
					else if B_Key(Dev_J)=90 and pos(lwrc$(Gbw_Btn_Tmp_Text$),"pgup")<=0 and pos(lwrc$(Gbw_Btn_Tmp_Text$),"pg up")<=0 then
						 Gbw_Btn_Tmp_Text$="[PgUp] "&trim$(Gbw_Btn_Tmp_Text$)
					else if B_Key(Dev_J)=91 and pos(lwrc$(Gbw_Btn_Tmp_Text$),"pgdn")<=0 and pos(lwrc$(Gbw_Btn_Tmp_Text$),"pg down")<=0 and pos(lwrc$(Gbw_Btn_Tmp_Text$),"pg dn")<=0 then
						 Gbw_Btn_Tmp_Text$="[PgDn] "&trim$(Gbw_Btn_Tmp_Text$)
					else if B_Key(Dev_J)=0 and pos(lwrc$(Gbw_Btn_Tmp_Text$),"enter")<=0 and trim$(Gbw_Btn_Tmp_Text$)<>"" then
						 Gbw_Btn_Tmp_Text$="[Enter] "&trim$(Gbw_Btn_Tmp_Text$)
					else if B_Key(Dev_J)=>1 and B_Key(Dev_J)<=12 and pos(lwrc$(Gbw_Btn_Tmp_Text$),"[f"&str$(B_Key(Dev_J))&"]")<=0 then
						 Gbw_Btn_Tmp_Text$="[F"&str$(B_Key(Dev_J))&"] "&trim$(Gbw_Btn_Tmp_Text$)
					end if  ! b_key(dev_j)=...
!         auto add fkey to label - end
				end if  ! ~gbw_noadd_keys
				 fnAddOneC(mat Gbw_Btn_Text$,Gbw_Btn_Tmp_Text$)
				 fnAddOneC(mat Gbw_Btn_Fkey$,B_Key$(Dev_J))
			end if  ! (mwb_button_row=1 and pos(b_lab$(dev_j),'(f)')>0) or (mwb_button_row=2 and pos(b_lab$(dev_j),'(f)')<=0)
		next Dev_J
		 fn_Get_Button_Fields(mat Gbw_Btn_Text$,val(Gbw_Win_Part$(Pw_Cols)),val(M_Row$),mat Gbw_Btn_Fkey$,mat Gbw_Field$)
		if Gbw_Win_Button_Pop then
			 fnArrayAddC(mat Gbw_Win_Button_Field$,mat Gbw_Win_Button_Field$,mat Gbw_Field$)
			 fnArrayAddC(mat Gbw_Win_Button_Text$,mat Gbw_Win_Button_Text$,mat Gbw_Btn_Text$)
		else
			print #Gbw_H_Window,FIELDS mat Gbw_Field$: mat Gbw_Btn_Text$ IOERR GBW_PRINT_ERR
		end if  ! gbw_win_button_pop   /   else
		 Auto_Add_Fkey_To_Label=0
	next Mwb_Button_Row
	goto GBW_XIT
	GBW_PRINT_ERR: !
		if Developer then
			for J=1 to udim(mat Gbw_Field$) : print 'gbw_field$('&str$(J)&')='&Gbw_Field$(J)&' - gbw_btn_text$('&str$(J)&')='&Gbw_Btn_Text$(J) : next J
			print 'could not print buttons - probably because the window is too narrow.'
			PAUSE
		end if  ! developer
	continue  ! GBW_PRINT_ERR
	GBW_XIT: !
fnend
def fn_Get_Button_Fields(mat Gbf_Label$,Gbf_Win_Width,Gbf_Row,mat Gbf_Fkey$,mat Gbf_Field$)
! todo: add button needs to auto move to correct position.  (far left i think)
! todo: add variable spacers between buttons,
! todo: right align buttons
	 Gbf_Count=udim(mat Gbf_Label$)
	 Gbf_Len_Max=0
	for Gbf_I=1 to Gbf_Count
		 Gbf_Label$(Gbf_I)=trim$(Gbf_Label$(Gbf_I))
		 Gbf_Len_Max=Max(Gbf_Len_Max,Len(Gbf_Label$(Gbf_I)))
	next Gbf_I
	if Gbf_Count>0 then
		if Gbf_Len_Max<=Int(Gbf_Win_Width/Gbf_Count) then
			if Gbf_Count<5 and Gbf_Len_Max*5<Gbf_Win_Width then
				 Gbf_Len_Button=Int(Gbf_Win_Width/5)
			else
				 Gbf_Len_Button=Gbf_Len_Max
			end if  ! gbf_count<5 and gbf_len_max*5<gbf_win_width   /   else
		else
			 Gbf_Len_Button=Int(Gbf_Win_Width/Gbf_Count)
		end if
	end if  ! gbf_count>0 then
	mat Gbf_Field$(Gbf_Count)
	for Gbf_I=1 to Gbf_Count
		 Gbf_Col=Gbf_Win_Width-(Gbf_Len_Button+1)+1 :  Gbf_Win_Width-=(Gbf_Len_Button+1)
		if env$("GUIMODE")="ON" then
			Gbf_Field$(Gbf_I)=(str$(Gbf_Row)&','&str$(Gbf_Col)&','&str$(Gbf_Len_Button)&'/Cc '&str$(Gbf_Len_Max)&',[Button]S,B'&Gbf_Fkey$(Gbf_I))
		else
			Gbf_Field$(Gbf_I)=(str$(Gbf_Row)&','&str$(Gbf_Col)&','&str$(Gbf_Len_Button)&'/Cc '&str$(Gbf_Len_Max)&',[Button_CUI]S,B'&Gbf_Fkey$(Gbf_I))
		end if
	next Gbf_I
fnend
def fn_openParent$*999(Open_Parent$*999; mat Op_Win_Part_Return$,Op_Force_Parent_None,Op_Restrain_Font_Size)
	dim Op_Win0_Fontsize(2)
	dim Op_Win_Part$(1)*256
	if env$("GUIMODE")="ON" and (Op_Force_Parent_None Or fn_enableParentNone) then  Op_Use_Parent_None=1 else  Op_Use_Parent_None=0
	 fn_windowParse(Open_Parent$,mat Op_Win_Part$)
	if Op_Win_Part$(Pw_Button_Text)<>'' then
		 fn_modifyWindowForButtons(mat Op_Win_Part$)
	end if  ! op_win_part$(pw_button_text)<>''
	 Open_Parent$=fn_windowBuild$(mat Op_Win_Part$,Op_Use_Parent_None,Op_Restrain_Font_Size)
	if Udim(mat Op_Win_Part_Return$)<>0 then
		mat Op_Win_Part_Return$(Udim(mat Op_Win_Part$))
		mat Op_Win_Part_Return$=Op_Win_Part$ ! soflow ignore
	end if  ! udim(mat op_win_part_return$)<>10
	 fn_openParent$=Open_Parent$
fnend
def fn_windowBuild$*1024(mat winPart$,enableParentNone,restrainFontSize; ___,return$*1024)
	 return$&=',Rows='&winPart$(Pw_Rows)
	 return$&=',Cols='&winPart$(Pw_Cols)
	if winPart$(Pw_Border)<>'' then
		 return$&=',Border='&winPart$(Pw_Border)
	end if
	if winPart$(Pw_Tabcap_Text)<>'' then
		 return$=return$&','&winPart$(Pw_Tabcap)&'='&winPart$(Pw_Tabcap_Text)
	end if  ! winPart$(pw_tabcap_text)<>''
	if winPart$(Pw_Name)<>'' and enableParentNone then
		 return$=return$&',Name='&winPart$(Pw_Name)
	end if  ! winPart$(pw_name)<>''
	if winPart$(Pw_Parent)<>'' and enableParentNone then
		 return$=return$&',Parent='&winPart$(Pw_Parent)
	end if  ! winPart$(pw_parent)<>''
	if winPart$(Pw_Position)<>'' and enableParentNone then
		 return$=return$&','&winPart$(Pw_Position)
	end if  ! winPart$(pw_position)<>''
	if winPart$(Pw_Modal)<>'' and enableParentNone then
		 return$=return$&','&winPart$(Pw_Modal)
	end if  ! winPart$(pw_modal)<>''
	if winPart$(Pw_No_Task_Bar)<>'' and enableParentNone then
		 return$=return$&','&winPart$(Pw_No_Task_Bar)
	end if  ! winPart$(pw_no_task_bar)<>''
	if winPart$(Pw_Fontsize)<>'' and enableParentNone then
		 return$=return$&',FontSize='&winPart$(Pw_Fontsize)
	end if  ! winPart$(pw_fontsize)<>''
	if winPart$(Pw_Border)<>'' and ~enableParentNone then
		 return$=return$&',Border='&winPart$(Pw_Border)
	end if  ! winPart$(pw_fontsize)<>''
	if enableParentNone then
		 return$=return$&',Relative'
		 return$=return$&',NoMaximize'
	end if  ! enableParentNone
	if pos(uprc$(return$),',FONTSIZE=')<=0 then
		dim Winbuild_Winnew_Fontsize(2)
		 File(0,"FONTSIZE", mat Winbuild_Win0_Fontsize)
		if restrainFontSize then
			 Winbuild_Winnew_Fontsize(1)=Max(Winbuild_Win0_Fontsize(1)+1,16)
			 Winbuild_Winnew_Fontsize(1)=Min(Winbuild_Win0_Fontsize(1)+1,22)
			 Winbuild_Winnew_Fontsize(2)=Max(Winbuild_Win0_Fontsize(2),8)
			 Winbuild_Winnew_Fontsize(2)=Min(Winbuild_Win0_Fontsize(2),10)
			 Winbuild_Height_Ratio=Winbuild_Win0_Fontsize(1)/Winbuild_Winnew_Fontsize(1)
			 Winbuild_Width_Ratio=Winbuild_Win0_Fontsize(2)/Winbuild_Winnew_Fontsize(2)
!     if jbowman then pr 'before winPart$(pw_srow)=';winPart$(pw_srow)
!     if jbowman then pr 'before winPart$(pw_scol)=';winPart$(pw_scol)
			 winPart$(Pw_Srow)=str$(Int(val(winPart$(Pw_Srow))*Winbuild_Height_Ratio )) !   +    val(winPart$(pw_rows))/2                 ))
			 winPart$(Pw_Scol)=str$(Int(val(winPart$(Pw_Scol))*Winbuild_Width_Ratio )) !   +     val(winPart$(pw_cols))/2             ))
			!       if jbowman then pr ' after winPart$(pw_srow)=';winPart$(pw_srow)
			!       if jbowman then pr ' after winPart$(pw_scol)=';winPart$(pw_scol) : pause
		else
			Winbuild_Winnew_Fontsize(1)=Winbuild_Win0_Fontsize(1)+1
			Winbuild_Winnew_Fontsize(2)=Winbuild_Win0_Fontsize(2)
		end if  ! restrainFontSize
		 env$("MONITOR1",mat Monitor1_Size)
		 Winbuild_Winnew_Fontsize(1)=Min( Int((Monitor1_Size(4)-Monitor1_Size(2)- 100)/val(winPart$(Pw_Rows)) ) - 2,Winbuild_Winnew_Fontsize(1))
		 return$(Inf:0)=',FontSize='&str$(Winbuild_Winnew_Fontsize(1)+1)&'x'&str$(Winbuild_Winnew_Fontsize(2))
	end if  ! pos(uprc$(return$),',FONTSIZE=')<=0
	 return$=return$&',SRow='&str$(Max(val(winPart$(Pw_Srow)),1))
	 return$=return$&',SCol='&winPart$(Pw_Scol)
	 return$=trim$(return$,',')
	 fn_windowBuild$=return$
fnend
def fn_modifyWindowForButtons(mat winPart$; ___,Mwb_Height_Mod)
	 Mwb_Height_Mod=fn_windowButtonRows(mat winPart$)
	 winPart$(Pw_Rows)=str$(val(winPart$(Pw_Rows))+Mwb_Height_Mod)
	 winPart$(Pw_Erow)=str$(val(winPart$(Pw_Erow))+Mwb_Height_Mod)

	do While val(winPart$(Pw_Srow))+val(winPart$(Pw_Rows))>24
		 winPart$(Pw_Srow)=str$(val(winPart$(Pw_Srow))-1)
	loop
fnend
def fn_windowButtonRows(mat Wbr_Win_Part$) ! wbr_
	! this function determines the number of button rows a window needs and returns that number.
	if pos(lwrc$(Wbr_Win_Part$(Pw_Button_Text)),'(f)')>0 Or Len(Wbr_Win_Part$(Pw_Button_Text))*1.2>val(Wbr_Win_Part$(Pw_Cols)) then  Wbr_Return=2 else  Wbr_Return=1
	 fn_windowButtonRows=Wbr_Return
fnend
def fn_enableParentNone
	fn_enableParentNone=1
fnend
def fn_windowParse(Pw_Source$*512,mat Pw_Data$) ! pw_   - makes .._win_part
	dim Pw_Part$(19)*512,Pw_Part$*512,Pw_Part_Lwrc$*512
	mat Pw_Data$(19)=('')
	 Pw_Data_Srow=Pw_Data_Scol=Pw_Data_Erow=Pw_Data_Ecol=Pw_Data_Rows=Pw_Data_Cols=Pw_Tmp_Has_Center=Pw_Tmp_Has_Left=Pw_Tmp_Has_Right=Pw_Tmp_Has_Top=Pw_Tmp_Has_Bottom=0 ! local numeric variables (pw_clear_var :P )
	 Str2mat(Pw_Source$,mat Pw_Part$,',')
	for Pw_Part_Item=1 to Udim(mat Pw_Part$)
		 Pw_Part$=Pw_Part$(Pw_Part_Item)
		 Pw_Part_Lwrc$=lwrc$(Pw_Part$)
		 Pw_Part_Len=Len(Pw_Part$)
		if Pw_Part_Lwrc$(1:4)='srow' then
			 Pw_Data$(Pw_Srow)=Pw_Part$(6:Pw_Part_Len)
			 Pw_Data_Srow=val(Pw_Data$(Pw_Srow)) CONV PW_CONV
		else if Pw_Part_Lwrc$(1:4)='scol' then
			 Pw_Data$(Pw_Scol)=Pw_Part$(6:Pw_Part_Len)
			 Pw_Data_Scol=val(Pw_Data$(Pw_Scol)) CONV PW_CONV
		else if Pw_Part_Lwrc$(1:4)='erow' then
			 Pw_Data$(Pw_Erow)=Pw_Part$(6:Pw_Part_Len)
			 Pw_Data_Erow=val(Pw_Data$(Pw_Erow)) CONV PW_CONV
		else if Pw_Part_Lwrc$(1:4)='ecol' then
			 Pw_Data$(Pw_Ecol)=Pw_Part$(6:Pw_Part_Len)
			 Pw_Data_Ecol=val(Pw_Data$(Pw_Ecol)) CONV PW_CONV
		else if Pw_Part_Lwrc$(1:4)='rows' then
			 Pw_Data$(Pw_Rows)=Pw_Part$(6:Pw_Part_Len)
			 Pw_Data_Rows=val(Pw_Data$(Pw_Rows)) CONV PW_CONV
		else if Pw_Part_Lwrc$(1:4)='cols' then
			 Pw_Data$(Pw_Cols)=Pw_Part$(6:Pw_Part_Len)
			 Pw_Data_Cols=val(Pw_Data$(Pw_Cols)) CONV PW_CONV
		else if Pw_Part_Lwrc$(1:3)='tab' then
			 Pw_Data$(Pw_Tabcap)='Tab'
			 Pw_Data$(Pw_Tabcap_Text)=Pw_Part$(5:Pw_Part_Len)
		else if Pw_Part_Lwrc$(1:7)='caption' then
			 Pw_Data$(Pw_Tabcap)='Caption'
			 Pw_Data$(Pw_Tabcap_Text)=Pw_Part$(9:Pw_Part_Len)
		else if Pw_Part_Lwrc$(1:6)='parent' then
			 Pw_Data$(Pw_Parent)=Pw_Part$(8:Pw_Part_Len)
		else if Pw_Part_Lwrc$(1:6)='border' then
			 Pw_Data$(Pw_Border)=Pw_Part$(8:Pw_Part_Len)
		else if Pw_Part_Lwrc$(1:11)='button.fkey' then
			 Pw_Data$(Pw_Button_Fkey)=Pw_Part$(13:Pw_Part_Len)
		else if Pw_Part_Lwrc$(1:11)='button.text' then
			 Pw_Data$(Pw_Button_Text)=Pw_Part$(13:Pw_Part_Len)
		else if Pw_Part_Lwrc$(1:11)='button.help' then
			 Pw_Data$(Pw_Button_Help)=Pw_Part$(13:Pw_Part_Len)
		else if Pw_Part_Lwrc$(1:8)='relative' then
			 Pw_Data$(Pw_Position)='relative'
		else if Pw_Part_Lwrc$(1:8)='absolute' then
			 Pw_Data$(Pw_Position)='absolute'
		else if Pw_Part_Lwrc$(1:5)='modal' then
			 Pw_Data$(Pw_Modal)='modal'
		else if Pw_Part_Lwrc$(1:11)='no_task_bar' then
			 Pw_Data$(Pw_No_Task_Bar)='no_task_bar'
		else if Pw_Part_Lwrc$(1:8)='fontsize' then
			 Pw_Data$(Pw_Fontsize)=Pw_Part$(10:Pw_Part_Len)
		else if Pw_Part_Lwrc$(1:4)='name' then
			 Pw_Data$(Pw_Name)=Pw_Part$(6:Pw_Part_Len)
		else if Pw_Part_Lwrc$(1:1)='n' then
			 Pw_Data$(Pw_Attr_N)=Pw_Part$(3:Pw_Part_Len)
		else if Pw_Part_Lwrc$='center' then
			 Pw_Tmp_Has_Center=1
		else if Pw_Part_Lwrc$='left' then
			 Pw_Tmp_Has_Left=1
		else if Pw_Part_Lwrc$='right' then
			 Pw_Tmp_Has_Right=1
		else if Pw_Part_Lwrc$='top' then
			 Pw_Tmp_Has_Top=1
		else if Pw_Part_Lwrc$='bottom' then
			 Pw_Tmp_Has_Bottom=1
		else
			!       Additional Parts May be defined here!
			if Debug Or Developer then print 'what is this "';Pw_Part$;'" of which you speak?' : PAUSE
		end if  ! pw_part_lwrc$(1:##)='...   /   else
	next Pw_Part_Item
	! center, left, right, top, bottom
	if Pw_Tmp_Has_Center then
		if Pw_Data_Rows then
			 Pw_Data_Srow=Int((Screen_Height-Pw_Data_Rows)/2)+1
			 Pw_Data$(Pw_Srow)=str$(Pw_Data_Srow)
		end if  ! pw_data_rows
		if Pw_Data_Cols then
			 Pw_Data_Scol=Int((Screen_Width-Pw_Data_Cols)/2)+1
			 Pw_Data$(Pw_Scol)=str$(Pw_Data_Scol)
		end if  ! pw_data_cols
	end if  ! pw_tmp_has_center
	if Pw_Tmp_Has_Left and Pw_Data_Rows then
		 Pw_Data_Scol=2
		 Pw_Data$(Pw_Scol)=str$(Pw_Data_Scol)
	end if  ! pw_tmp_has_left and pw_data_rows
	if Pw_Tmp_Has_Right and Pw_Data_Cols then
		 Pw_Data_Scol=Screen_Width-Pw_Data_Cols
		 Pw_Data$(Pw_Scol)=str$(Pw_Data_Scol)
	end if  ! pw_tmp_has_right and pw_data_cols
	if Pw_Tmp_Has_Top then
		 Pw_Data_Srow=2
		 Pw_Data$(Pw_Srow)=str$(Pw_Data_Srow)
	end if  ! pw_tmp_has_top
	if Pw_Tmp_Has_Bottom then
		 Pw_Data_Srow=Screen_Height-Pw_Data_Rows-1
		 Pw_Data$(Pw_Srow)=str$(Pw_Data_Srow)
	end if  ! pw_tmp_has_bottom
	!
	! fill in the gaps
	if Pw_Data_Rows=0 then
		if ~Pw_Data_Erow and ~Pw_Data_Rows and Developer then print 'pw_data_erow=pw_data_rows=0 - trouble' : PAUSE
		 Pw_Data_Rows=Pw_Data_Erow-Pw_Data_Srow+1
		 Pw_Data$(Pw_Rows)=str$(Pw_Data_Rows)
	end if  ! pw_data_rows=0
	if Pw_Data_Cols=0 then
		if ~Pw_Data_Cols and ~Pw_Data_Ecol and Developer then print 'pw_data_erow=pw_data_rows=0 - trouble' : PAUSE
		 Pw_Data_Cols=Pw_Data_Ecol-Pw_Data_Scol+1
		 Pw_Data$(Pw_Cols)=str$(Pw_Data_Cols)
	end if  ! pw_data_cols=0
	if Pw_Data_Erow=0 then
		 Pw_Data_Erow=Pw_Data_Srow+Pw_Data_Rows
		 Pw_Data$(Pw_Erow)=str$(Pw_Data_Erow)
	end if  ! pw_data_erow=0
	if Pw_Data_Ecol=0 then
		 Pw_Data_Ecol=Pw_Data_Scol+Pw_Data_Cols
		 Pw_Data$(Pw_Ecol)=str$(Pw_Data_Ecol)
	end if  ! pw_data_ecol=0
	GOTO PW_XIT
	PW_CONV: !
	if Debug then print 'conv on ';Line;' of fn_windowParse.'
	if Developer then PAUSE
	continue  ! pw_conv
	PW_XIT: !
fnend  ! fn_windowParse
def fn_Client_Os_Path$*265(Cs_Os_Path$*256)
	if Cs_Os_Path$(1:3)="@::" then
		 fn_Client_Os_Path$=Cs_Os_Path$
	else if trim$(env$("CLIENT_OS_PATH")&env$("SERVER_OS_PATH")&env$("CLIENT_DRIVE_N_PATH")&env$("SERVER_DRIVE_N_PATH"))<>"" then
		 Cs_Os_Path$=uprc$(Os_Filename$(Cs_Os_Path$)) ERROR IGNORE
		if trim$(env$("CLIENT_DRIVE_N_PATH"))<>"" and trim$(env$("SERVER_DRIVE_N_PATH"))<>"" then
			Cs_Os_Path$=srep$(Cs_Os_Path$,uprc$(env$("SERVER_DRIVE_N_PATH")),env$("CLIENT_DRIVE_N_PATH"))
		end if
		if trim$(env$("CLIENT_OS_PATH"))<>"" and trim$(env$("SERVER_OS_PATH"))<>"" then
			Cs_Os_Path$=srep$(Cs_Os_Path$,uprc$(env$("SERVER_OS_PATH")),env$("CLIENT_OS_PATH"))
		end if
		 fn_Client_Os_Path$=Cs_Os_Path$
	else
		fn_Client_Os_Path$=Os_Filename$(Cs_Os_Path$)
	end if
fnend
def fn_arrayReverse$(mat ar_reverse_me$)
  dim ar_reverse_tmp$(1)*40
  ar_reverse_count=udim(mat ar_reverse_me$)
  mat ar_reverse_tmp$(ar_reverse_count)=ar_reverse_me$
  for ar_reverse_item=1 to ar_reverse_count
    ar_reverse_me$(ar_reverse_item)=ar_reverse_tmp$(udim(ar_reverse_me$)-ar_reverse_item+1)
  next ar_reverse_item
fnend