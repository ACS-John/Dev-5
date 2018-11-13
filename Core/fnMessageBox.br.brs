library 's:\Core\Library': fngethandle
library 's:\Core\Library': fnArrayAddC
library 's:\Core\Library': fnAddOneC
library 's:\Core\Library': fnArrayReverseC
library 's:\Core\Library': fnChrCount
library 's:\Core\Library': fnGetPp


def Fn_Messagebox(Mbx_Message$*2048; Mbx_Type,Mbx_Title$*200,Mbx_Nogui$*28) ! mbx_
	Orig_Mb_Fkey=Fkey
! mbx_name$*20 - if included than a checkbox for "[ ] don't ask me again next time" will be added to the message
	if ~Mbx_Setup then 
		Mbx_Setup=1
		dim Mbx_Dont_Ask_Again_Text$*40
		dim Mbx_Win_Data$(1)*128,Mbx_Win$*2048,Mbx_Name$*40,Mbx_Button_Fkey_List$*256
		dim Mbx_File_Os$(1)*256
		dim Mbx_File_V$(1)*256
		dim Mbx_Win_Field$(1)*40,Mbx_Win_Text$(1)*40
!   mbx_dont_ask_again_text$='Remember my answer next time.'
		Mbx_Wait_Time=15*60 ! if jbowman then mbx_wait_time=3 else mbx_wait_time=15*60
	end if  ! ~mbx_setup
	if Lwrc$(Env$('Messagebox_Always_Ask'))='yes' then Mbx_Always_Ask=1 ELSE Mbx_Always_Ask=0 ! note this line - this line forces the questions to be ask even if they have choosen not to remember prior answer.
	Mbx_Name$=Mbx_Nogui$
	Mbx_Message$=Srep$(Mbx_Message$,Crlf$,Lf$)
	Mbx_Message$=Srep$(Mbx_Message$,Cr$,Lf$)
	Mbx_Message$=Srep$(Mbx_Message$,'\n',Lf$)
	Fn_Mbx_Size(Mbx_Message$,Mb3_Text_Rows,Mb3_Cols)
	Mb3_Text_Rows=Max(Mb3_Text_Rows,5)
	Mb3_Parent_Rows=Mb3_Text_Rows+2
	Mb3_Cols=Min(Mb3_Cols,80)
	Mbx_Button_Default=Fn_Mbx_Button_Default(Mbx_Type)
	Mbx_Icon$=Fn_Mbx_Icon$(Mbx_Type)
	Mbx_Button_Count=Fn_Mbx_Button(Mbx_Type,Mbx_Button_Text$,Mat Mbx_Button_Return)
	if Mbx_Name$<>'' then 
		Mb3_Parent_Rows+=1 ! add line for dont ask again text and a blank line
		if Udim(Mat Mbx_Button_Return)=1 then 
			Mbx_Dont_Ask_Again_Text$='Do not notify me again.'
		ELSE 
			Mbx_Dont_Ask_Again_Text$='Remember my answer next time.'
		end if 
		Mb3_Cols=Max(Mb3_Cols,Len(Mbx_Dont_Ask_Again_Text$)+10)
		Mbx_Dont_Ask_Row=Mb3_Parent_Rows-1 ! mbx_dont_ask_row is the row to print the dont ask again text on
	end if  ! mbx_name$<>''
	Mbx_Button_Fkey_List$=''
	MAT Mbx_Button_Fkey(Mbx_Button_Count)
	FOR Mbx_Button_Fkey_Item=1 TO Mbx_Button_Count
		Mbx_Button_Fkey(Mbx_Button_Fkey_Item)=2010+Mbx_Button_Fkey_Item
		Mbx_Button_Fkey_List$(Inf:0)=Str$(Mbx_Button_Fkey(Mbx_Button_Fkey_Item))&';'
	NEXT Mbx_Button_Fkey_Item
	Mbx_Button_Fkey_List$=Mbx_Button_Fkey_List$(1:Len(Mbx_Button_Fkey_List$)-1) ! remove the last ;
! 
	if Mbx_Type<>0 then PRINT 'mbx_type<>0 - something must be wrong' : PAUSE 
! 
	if Mbx_Name$<>'' then 
		Mbx_Name$='mbx_'&Trim$(Mbx_Name$)&'_'&Str$(Mb3_Parent_Rows)&'x'&Str$(Mb3_Cols)
		Mbx_Win$='name='&Mbx_Name$&','
		Mbx_Checked=Fnregunum_Get(Mbx_Name$)
	ELSE 
		Mbx_Checked=0
	end if  ! mbx_name$<>''   /   else 
	Mbx_File_Count=Fn_Mbx_Get_File_List(Mbx_H_Text,Mbx_Message$,Mbx_H_Cols,Mat Mbx_File_Os$,Mat Mbx_File_V$)
	if Mbx_Always_Ask Or Mbx_Checked<=9999 then 
		MAT Mbx_Win_Field$(1)=('')
		MAT Mbx_Win_Text$(1)=('')
		Mbx_Win$=''
		Mbx_Timeout=0
!   if mbx_checked>9999 then mbx_dont_ask_again_text$='^'&mbx_dont_ask_again_text$
		Mbx_Win$(Inf:0)='Caption='&Srep$(Mbx_Title$,',','.')
		Mbx_Win$(Inf:0)=',rows='&Str$(Mb3_Parent_Rows)
		Mbx_Win$(Inf:0)=',cols='&Str$(Mb3_Cols)
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
		open #Mbx_H_Win:=fngethandle: Fn_open_Parent$(Mbx_Win$,Mat Mbx_Win_Data$,1,1),DISPLAY,OUTPUT 
		Fn_Generate_Buttons_For_Window(Mat Mbx_Win_Data$,Mbx_H_Win, 1,Mat Mbx_Win_Field$,Mat Mbx_Win_Text$)
		fnArrayReverseC(Mat Mbx_Win_Field$)
		fnArrayReverseC(Mat Mbx_Win_Text$)
		if Exists('Icons\'&Mbx_Icon$&'.png') then PRINT #Mbx_H_Win,FIELDS '1,1,P 4/8,[W]': 'Icons\'&Mbx_Icon$&'.png:isotropic'
		Mbx_H_Cols=Val(Mbx_Win_Data$(Pw_Cols))
		Mbx_H_Text_Rows=Mb3_Text_Rows
		Mbx_H_Text_Cols=Mbx_H_Cols-10
		open #Mbx_H_Text:=fngethandle: 'Modal,Parent='&Str$(Mbx_H_Win)&',Border=None,SRow=1,SCol=10,Rows='&Str$(Mbx_H_Text_Rows)&',Cols='&Str$(Mbx_H_Text_Cols),DISPLAY,OUTPUT 
! if jbowman then print 'mbx_message$='&mbx_message$ : pause
		PRINT #Mbx_H_Text,FIELDS '1,1,'&Fn_Mbx_Text_Size$(Mbx_H_Text_Cols,Mbx_H_Text_Rows)&',[T]S,49': Mbx_Message$
		if Mbx_Name$<>'' then 
			fnAddOneC(Mat Mbx_Win_Field$,Str$(Mbx_Dont_Ask_Row)&',10,Check '&Str$(Len(Mbx_Dont_Ask_Again_Text$)+1)&',[W]ae,2020')
			fnAddOneC(Mat Mbx_Win_Text$,Rpt$('^',Min(1,Mbx_Checked-10000))&Mbx_Dont_Ask_Again_Text$)
			Mbx_Win_Which_Don_Ask=Udim(Mat Mbx_Win_Text$)
			if Mbx_Checked-10000>0 then Mbx_Button_Default=Srch(Mat Mbx_Button_Fkey,Mbx_Checked-10000)
		end if  ! mbx_name$<>''
		FOR Mbx_Win_Field_Item=1 TO Udim(Mat Mbx_Win_Field$)
			Mbx_Win_Field$(Mbx_Win_Field_Item)=Srep$(Mbx_Win_Field$(Mbx_Win_Field_Item),'[Button]','[Button]ae')
		NEXT Mbx_Win_Field_Item
		Curfld(Mbx_Button_Default)
		DO 
			Mb_X_Click=0
			RINPUT #Mbx_H_Win,SELECT Mat Mbx_Win_Field$,ATTR '[A]',WAIT=Mbx_Wait_Time: Mat Mbx_Win_Text$ TIMEOUT IGNORE
			if Fkey=93 Or Fkey=99 then 
				GOSUB MBX_DEFAULT_FKEY 
				Mb_X_Click=Mb_X
			ELSE IF Fkey=101 then 
				Mbx_Timeout=1
				GOSUB MBX_DEFAULT_FKEY
			ELSE IF Fkey=2020 then 
				if Mbx_Win_Text$(Mbx_Win_Which_Don_Ask)(1:1)='^' then 
					Mbx_Win_Text$(Mbx_Win_Which_Don_Ask)(1:1)=''
				ELSE 
					Mbx_Win_Text$(Mbx_Win_Which_Don_Ask)(0:0)='^'
				end if  ! mbx_win_text$(mbx_win_which_don_ask)(1:1)='^'   /   else 
			ELSE IF Fkey=33 And Mbx_File_Count>0 then ! Alt+F
				Fn_Mbx_File_Menu(Mbx_Title$,Mbx_File_Count,Mbx_H_Text_Cols,Mat Mbx_File_Os$,Mat Mbx_File_V$,Mbx_Wait_Time)
			ELSE IF Fkey=46 then ! Alt+C
				Setenv('ClipBoard',Srep$(Mbx_Message$,Lf$,Crlf$))
			ELSE IF Fkey=99 And Mbx_Button_Count=1 then 
				Fkey(Mbx_Button_Fkey(1))
			end if  ! fkey=...
		LOOP Until Srch(Mat Mbx_Button_Fkey,Fkey)>0
		if Mbx_Name$<>'' And Fkey<>101 then 
			if Mbx_Win_Text$(Mbx_Win_Which_Don_Ask)(1:1)='^' then Mbx_Checked=1 ELSE Mbx_Checked=0
			if Mbx_Checked then 
				Fnregunum_Put(Mbx_Name$,10000+Fkey)
			ELSE 
				Fnregunum_Put(Mbx_Name$,0)
			end if  ! mbx_checked   /   else 
		end if  ! mbx_name$=''
		CLOSE #Mbx_H_Win: 
	ELSE 
!   print 'returning with fkey '&str$(mbx_checked-10000)&' from memory'
		Fkey(Mbx_Checked-10000)
!   pause
	end if  ! mbx_always_ask or mbx_checked<=9999   /   else 
	GOTO MBX_XIT
MBX_DEFAULT_FKEY: ! 
	if Curfld<=Mbx_Button_Count And Srch(Mat Mbx_Button_Fkey,Mbx_Button_Fkey(Curfld))>0 then 
		Fkey(Mbx_Button_Fkey(Curfld))
	ELSE 
		Fkey(Mbx_Button_Fkey(Mbx_Button_Default))
	end if  ! curfld<=mbx_button_count and srch(mat mbx_button_fkey,mbx_button_fkey(curfld))>0   /   else 
	RETURN  ! MBX_DEFAULT_FKEY
MBX_XIT: ! 
	if Mb_X_Click then 
		Fn_Messagebox=Mb_X_Click 
	ELSE 
		Fn_Messagebox=Mbx_Button_Return(Srch(Mat Mbx_Button_Fkey,Fkey))
	end if
	if Mbx_Timeout then 
		Fkey(101)
	end if
	if Fkey<>93 And Fkey<>101 then 
		Fkey(Orig_Mb_Fkey)
	end if
	Orig_Mb_Fkey=0
fnend  ! fn_messagebox
def Fn_Mbx_File_Menu(Mbxfm_Title$*80,Mbxfm_File_Count,Mbxfm_H_Text_Cols,Mat Mbxfm_File_Os$,Mat Mbxfm_File_V$,Mbxfm_Wait_Time) ! mbxfm_
! 
	dim Mbxfm_Action_Win$*256,Mbxfm_Win_Action_Data$(1)*128
	dim Mbxfm_File_Path$*256,Mbxfm_File_Name_Base$*128,Mbxfm_File_Name_Extension$*128
	dim Mbxfm_H_Tab(2)
	dim Mbxfm_Action_Tab$*256,Mbxfm_Selection$*256
	dim Mbxfm_File_Field$(1)*20
	Mbxfm_Action_Win$=Mbxfm_S$=''
	if Mbxfm_File_Count>1 then Mbxfm_S$='s'
	Mbxfm_Action_Win$(Inf:0)='Caption='&Mbxfm_Title$&' - File'&Mbxfm_S$
	Mbxfm_Action_Win$(Inf:0)=',center'
	Mbxfm_Action_Win$(Inf:0)=',rows='&Str$(Mbxfm_File_Count+3)
	Mbxfm_Action_Win$(Inf:0)=',cols='&Str$(Mbxfm_Win_Action_Cols:=Max(40,Mbxfm_H_Text_Cols)+2)
	Mbxfm_Action_Win$(Inf:0)=',button.text=Close;(f)Copy;(f)open;(f)open Folder'
	Mbxfm_Action_Win$(Inf:0)=',button.fkey=99;2001;2002;2003'
	Mbxfm_Action_Win$(Inf:0)=',parent=none'
	if ~Developer then Mbxfm_Action_Win$(Inf:0)=',modal,Nomaximize'
	open #Mbxfm_H_Win:=fngethandle: Fn_open_Parent$(Mbxfm_Action_Win$,Mat Mbxfm_Win_Action_Data$,1),DISPLAY,OUTPUT 
	Mbxfm_Action_Tab$='SRow=2,SCol=2,Rows='&Str$(Mbxfm_File_Count+0)&',Cols='&Str$(Mbxfm_Win_Action_Cols-2)&',Parent='&Str$(Mbxfm_H_Win)
	Fn_Generate_Buttons_For_Window(Mat Mbxfm_Win_Action_Data$,Mbxfm_H_Win)
	open #Mbxfm_H_Tab(1):=fngethandle: Fn_open_Parent$(Mbxfm_Action_Tab$&',Tab=OS Path'&Mbxfm_S$,Mat Mbxfm_Tab_Data$,1),DISPLAY,OUTPUT 
	open #Mbxfm_H_Tab(2):=fngethandle: Fn_open_Parent$(Mbxfm_Action_Tab$&',Tab=Virtual Path'&Mbxfm_S$,Mat Mbxfm_Tab_Data$,1),DISPLAY,OUTPUT 
	MAT Mbxfm_File_Field$(Mbxfm_File_Count)
	FOR Mbxfm_File_Item=1 TO Mbxfm_File_Count
		Mbxfm_File_Field$(Mbxfm_File_Item)=Str$(Mbxfm_File_Item)&',1,40/C '&Mbxfm_Win_Action_Data$(Pw_Cols)&',[M]'
	NEXT Mbxfm_File_Item
	Mbxfm_Curtab=Fnregunum_Get('mbxfm_CurTab')
	if Mbxfm_Curtab<=0 then Mbxfm_Curtab=2
	DO 
		Curfld(Mbxfm_Curfld)
		if Mbxfm_Curtab=1 then 
			RINPUT #Mbxfm_H_Tab(Mbxfm_Curtab),SELECT Mat Mbxfm_File_Field$, ATTR '[L]',WAIT=Mbxfm_Wait_Time: Mat Mbxfm_File_Os$ TIMEOUT IGNORE
			Mbxfm_Selection$=Mbxfm_File_Os$(Curfld)
		ELSE ! if mbxfm_curtab=2 then
			RINPUT #Mbxfm_H_Tab(Mbxfm_Curtab),SELECT Mat Mbxfm_File_Field$, ATTR '[L]',WAIT=Mbxfm_Wait_Time: Mat Mbxfm_File_V$ TIMEOUT IGNORE
			Mbxfm_Selection$=Mbxfm_File_V$(Curfld)
		end if  ! mbxfm_curtab=1   /   else 
		Mbxfm_Curfld=Curfld
		if Fkey=92 then 
			if Mbxfm_Curtab=1 then Mbxfm_Curtab=2 ELSE Mbxfm_Curtab=1
		ELSE IF Fkey=101 then 
			Fkey(99)
		ELSE IF Fkey=2001 then 
			Setenv('ClipBoard',Mbxfm_Selection$)
		ELSE IF Fkey=2002 then 
			Fn_Exe('-c -w','VB32\WinShell',Mbxfm_File_Os$(Curfld))
		ELSE IF Fkey=2003 then 
			fnGetPp(Mbxfm_File_Os$(Curfld),Mbxfm_File_Path$,Mbxfm_File_Name_Base$,Mbxfm_File_Name_Extension$)
			EXECUTE "System -c -w explorer /select,"&Srep$(Mbxfm_File_Os$(Curfld),"@::","")&'"'             ! fn_exe MIGHT NOT BE A GOOD IDEA HERE!
		end if  ! fkey=92   /   101   /   2001   /   2002   /   2003
	LOOP Until Fkey=99 Or Fkey=93
	Fnregunum_Put('mbxfm_CurTab',Mbxfm_Curtab)
	CLOSE #Mbxfm_H_Win: 
fnend  ! fn_mbx_file_menu
def Fn_Mbx_Get_File_List(Mbxht_H_Parent,Mbxht_Text$*2048,Mbxht_Cols,Mat Mbxht_File_Os$,Mat Mbxht_File_V$) ! mbxht_
	dim Mbxht_Line$(1)*1024
	Str2mat(Mbxht_Text$,Mat Mbxht_Line$) ! No Paramater needed here LF of all types
	MAT Mbxht_File_Os$(0)
	MAT Mbxht_File_V$(0)
	FOR Mbxht_Line_Item=1 TO Udim(Mat Mbxht_Line$)
		if Mbxht_Line$(Mbxht_Line_Item)(1:2)='\\' Or Mbxht_Line$(Mbxht_Line_Item)(2:2)=':' then ! the line specifies a file
			Os_Filename$(Mbxht_Line$(Mbxht_Line_Item)) ERROR 30260 ! Check for "Bad File Names"
			fnAddOneC(Mat Mbxht_File_V$,Mbxht_Line$(Mbxht_Line_Item),0,1)
			fnAddOneC(Mat Mbxht_File_Os$,Fn_Client_Os_Path$(Mbxht_Line$(Mbxht_Line_Item)),0,1) ! Fn_Os_Filename$ - FN_Client_OS_Path does this!
		end if  ! mbxht_line$(mbxht_line_item)(1:2)='\\' or mbxht_line$(mbxht_line_item)(2:2)=':'
	NEXT Mbxht_Line_Item
	Fn_Mbx_Get_File_List=Udim(Mat Mbxht_File_Os$)
fnend  ! fn_mbx_get_file_list
def Fn_Mbx_Text_Size$(Mts_Cols,Mts_Rows)
	Mts_Cols_Original=Mts_Cols
	Mts_Rows_Original=Mts_Rows
	Fn_Mbx_Text_Size$=Str$(Mbx_H_Text_Cols*Mbx_H_Text_Rows)&"/C" ! str$(mts_cols_original*mts_rows_original)&'/Cl '&str$(mts_cols*mts_rows)
fnend  ! fn_mbx_text_size$
def Fn_Mbx_Size(&Mbxs_Message$,&Mbxs_Rows,&Mbxs_Cols) ! mbxs_
	dim Mbxs_Line$(1)*1024
	Str2mat(Mbxs_Message$,Mat Mbxs_Line$) ! ,LF$ is not needed because STR2MAT checks all types or LF
	Mbxs_Rows=Udim(Mat Mbxs_Line$)
	Mbxs_Cols=0
	FOR Mbxs_Line_Item=1 TO Mbxs_Rows
		Mbxs_Cols=Max(Mbxs_Cols,Len(Mbxs_Line$(Mbxs_Line_Item)))
		Mbxs_Cols+=fnChrCount(Mbxs_Line$(Mbxs_Line_Item),Tab$)*4
	NEXT Mbxs_Line_Item
	if Mbxs_Cols>140 then 
		Mbxs_Cols=Ceil(Mbxs_Cols*.78)
	ELSE IF Mbxs_Cols>90 then 
		Mbxs_Cols=Ceil(Mbxs_Cols*.82)
	ELSE IF Mbxs_Cols>60 then 
		Mbxs_Cols=Ceil(Mbxs_Cols*.86)
	ELSE IF Mbxs_Cols>30 then 
		Mbxs_Cols=Ceil(Mbxs_Cols*.90)
! else 
!   mbxs_cols=ceil(mbxs_cols*1.0)
	end if 
	Mbxs_Cols=Max(Mbxs_Cols,10)
	Mbxs_Cols+=13
fnend  ! fn_mbx_size
def Fn_Mbx_Button_Default(&Mbx_Type)
	if Mbx_Type>=Mb_Button3_Default then 
		Mbx_Type-=Mb_Button3_Default
		Mbx_Button_Default=3
	ELSE IF Mbx_Type>=Mb_Button2_Default then 
		Mbx_Type-=Mb_Button2_Default
		Mbx_Button_Default=2
	ELSE 
		Mbx_Button_Default=1
	end if  ! mbx_type
	Fn_Mbx_Button_Default=Mbx_Button_Default
fnend  ! fn_mbx_button_default
def Fn_Mbx_Icon$(&Mbx_Type)
	if Mbx_Type>=Mb_Information then 
		Mbx_Type-=Mb_Information
		Mbx_Icon$='information'
	ELSE IF Mbx_Type>=Mb_Exclamation then 
		Mbx_Type-=Mb_Exclamation
		Mbx_Icon$='exclamation'
	ELSE IF Mbx_Type>=Mb_Question then 
		Mbx_Type-=Mb_Question
		Mbx_Icon$='question'
	ELSE IF Mbx_Type>=Mb_Stop then 
		Mbx_Type-=Mb_Stop
		Mbx_Icon$='stop'
	ELSE 
!   if developer or debug then print bell;'no icon selected for messagegox - default of "information" will be used.'
!   if developer then pause
		Mbx_Icon$='information'
	end if 
	Fn_Mbx_Icon$=Mbx_Icon$
fnend  ! fn_mbx_icon$
def Fn_Mbx_Button(&Mbxb_Type,&Mbxb_Button_Text$,Mat Mbxb_Button_Return) ! mbxb_
	MAT Mbxb_Button_Return(1) : Mbxb_Button_Text$="?"&Str$(Mbxb_Type): Mbxb_Button_Return(1)=Mb_Ok
	Mbxb_Button_Text$=''
	if Mbxb_Type>=Mb_Retrycancel then 
		Mbxb_Type-=Mb_Retrycancel : Mbxb_Button_Text$='Retry;Cancel' 
		MAT Mbxb_Button_Return(2) : Mbxb_Button_Return(1)=Mb_Retry 
		Mbxb_Button_Return(2)=Mb_Cancel : Mb_X=Mb_Cancel
	ELSE IF Mbxb_Type>=Mb_Yesno then 
		Mbxb_Type-=Mb_Yesno
		Mbxb_Button_Text$='Yes;No'
		MAT Mbxb_Button_Return(2) : Mbxb_Button_Return(1)=Mb_Yes 
		Mbxb_Button_Return(2)=Mb_No : Mb_X=Mb_No
	ELSE IF Mbxb_Type>=Mb_Yesnocancel then 
		Mbxb_Type-=Mb_Yesnocancel
		Mbxb_Button_Text$='Yes;No;Cancel'
		MAT Mbxb_Button_Return(3) : Mbxb_Button_Return(1)=Mb_Yes 
		Mbxb_Button_Return(2)=Mb_No : Mbxb_Button_Return(3)=Mb_Cancel 
		Mb_X=Mb_Cancel
	ELSE IF Mbxb_Type>=Mb_Abortretryignore then 
		Mbxb_Type-=Mb_Abortretryignore
		Mbxb_Button_Text$='Abort;Retry;Ignore'
		MAT Mbxb_Button_Return(3) : Mbxb_Button_Return(1)=Mb_Abort 
		Mbxb_Button_Return(2)=Mb_Retry : Mbxb_Button_Return(3)=Mb_Ignore 
		Mb_X=Mb_Abort
	ELSE IF Mbxb_Type>=Mb_Okcancel then 
		Mbxb_Type-=Mb_Okcancel
		Mbxb_Button_Text$='OK;Cancel'
		MAT Mbxb_Button_Return(2) : Mbxb_Button_Return(1)=Mb_Ok 
		Mbxb_Button_Return(2)=Mb_Cancel : Mb_X=Mb_Cancel
	ELSE 
		MAT Mbxb_Button_Return(1) : Mbxb_Button_Return(1)=Mb_Ok
		Mbxb_Type-=Mb_Okonly
		Mbxb_Button_Text$='OK' 
		Mb_X=Mb_Ok
	end if 
	Fn_Mbx_Button=Udim(Mat Mbxb_Button_Return)
fnend  ! fn_mbx_button



def Fn_Exe(Exe_Parameters$*32,Exe_Name$*256;Exe_Filename$*256,Exe_Add_Parameters$*2048)
	Exe_Name$=Srep$(Exe_Name$,"[PKZIP]",Env$("Client_Zip_Program")) 
	Exe_Name$=Srep$(Exe_Name$,"[PKUNZIP]",Env$("Client_UnZip_Program"))
	Exe_Quote$='"' 
	if Pos(Exe_Parameters$,"QUOTE=NONE")>0 Or Uprc$(Trim$(Exe_Name$))(1:5)="START" then 
		Exe_Quote$='' 
		Exe_Parameters$=Srep$(Exe_Parameters$,"QUOTE=NONE","") 
		Exe_Parameters$=Trim$(Exe_Parameters$)
	end if
	if Uprc$(Trim$(Exe_Name$))(1:5)<>"START" then 
		Exe_Name$=Fn_Client_Os_Path$(Exe_Name$)
	end if
	if Trim$(Exe_Filename$)<>"" then 
		Exe_Filename$=Fn_Client_Os_Path$(Exe_Filename$)
	end if
	Exe_Filename$=Srep$(Exe_Filename$,"@::","") 
	Exe_Name$=Srep$(Exe_Name$,"@::","")
	if Trim$(Exe_Filename$)<>"" And Exe_Filename$(1:1)<>Exe_Quote$ And Exe_Quote$<>"" then 
		Exe_Filename$=Exe_Quote$&Trim$(Exe_Filename$)&Exe_Quote$
	end if
	if Pos(Exe_Name$," ")>0 then 
		EXECUTE "SYS "&Exe_Parameters$&' "'&Trim$(Trim$(Exe_Name$)&'" '&Trim$(Exe_Filename$))&" "&Trim$(Exe_Add_Parameters$) 
	ELSE 
		EXECUTE "SYS "&Exe_Parameters$&' '&Trim$(Trim$(Exe_Name$)&' '&Trim$(Exe_Filename$))&" "&Trim$(Exe_Add_Parameters$)
	end if
fnend 

DEF Fn_Generate_Buttons_For_Window(Mat Gbw_Win_Part$,Gbw_H_Window; Gbw_Noadd_Keys,Mat Gbw_Win_Button_Field$,Mat Gbw_Win_Button_Text$) ! gbw_
! (f) is for Features - the top row of buttons is for program features, the bottom for of buttons is for navigation.
	DIM Gbw_Field$(1)*40
	DIM Gbw_Btn_Text$(1)*80,Gbw_Btn_Tmp_Text$*80,Gbw_Btn_Fkey$(1)
	IF Udim(Mat Gbw_Win_Button_Field$)>0 THEN LET Gbw_Win_Button_Pop=1 ELSE LET Gbw_Win_Button_Pop=0
	IF Gbw_Win_Button_Pop THEN 
		MAT Gbw_Win_Button_Field$(0)
		MAT Gbw_Win_Button_Text$(0)
	END IF  ! gbw_win_button_pop
	LET Gbw_Height_Mod=Fn_Window_Button_Rows(Mat Gbw_Win_Part$)
	LET Gbw_Loop_Start=1
	LET Gbw_Loop_End=Gbw_Height_Mod
	IF Gbw_Height_Mod=1 And Pos(Gbw_Win_Part$(Pw_Button_Text),'(f)')<=0 THEN LET Gbw_Loop_Start=2 : LET Gbw_Loop_End=2
	FOR Mwb_Button_Row=Gbw_Loop_Start TO Gbw_Loop_End
! 
		LET Buttons_Row=Val(Gbw_Win_Part$(Pw_Rows))-2+Mwb_Button_Row
! 
		LET Str2mat(Gbw_Win_Part$(Pw_Button_Text),Mat B_Lab$,';')
		LET Str2mat(Gbw_Win_Part$(Pw_Button_Fkey),Mat B_Key$,';')
		LET Tot_Keys=Udim(B_Key$)
		MAT B_Lab$(Tot_Keys)
		MAT B_Key(Tot_Keys)
		FOR Ncount=1 TO Tot_Keys ! build Mat B_Key from Mat B_Key$
			LET B_Key(Ncount)=Val(B_Key$(Ncount)) CONV IGNORE
		NEXT Ncount
		LET M_Row$=Str$(Buttons_Row)
		LET Status_String$=B_String$=""
		PRINT #Gbw_H_Window,FIELDS M_Row$&",1,C "&Gbw_Win_Part$(Pw_Cols)&",[W]": "" ERROR IGNORE ! Delete Button Row
		MAT Gbw_Btn_Text$(0)
		MAT Gbw_Btn_Fkey$(0)
		FOR Dev_J=Udim(Mat B_Lab$) TO 1, STEP -1
			LET Gbw_Btn_Tmp_Text$=Srep$(Srep$(B_Lab$(Dev_J),'(f)',''),'(n)','')
			IF (Mwb_Button_Row=1 And Pos(B_Lab$(Dev_J),'(f)')>0) Or (Mwb_Button_Row=2 And Pos(B_Lab$(Dev_J),'(f)')<=0) THEN 
				IF ~Gbw_Noadd_Keys THEN 
!         auto add fkey to label - top
					IF B_Key(Dev_J)=99 And Pos(Lwrc$(Gbw_Btn_Tmp_Text$),"esc")<=0 THEN 
						LET Gbw_Btn_Tmp_Text$="[Esc] "&Trim$(Gbw_Btn_Tmp_Text$)
					ELSE IF B_Key(Dev_J)=90 And Pos(Lwrc$(Gbw_Btn_Tmp_Text$),"pgup")<=0 And Pos(Lwrc$(Gbw_Btn_Tmp_Text$),"pg up")<=0 THEN 
						LET Gbw_Btn_Tmp_Text$="[PgUp] "&Trim$(Gbw_Btn_Tmp_Text$)
					ELSE IF B_Key(Dev_J)=91 And Pos(Lwrc$(Gbw_Btn_Tmp_Text$),"pgdn")<=0 And Pos(Lwrc$(Gbw_Btn_Tmp_Text$),"pg down")<=0 And Pos(Lwrc$(Gbw_Btn_Tmp_Text$),"pg dn")<=0 THEN 
						LET Gbw_Btn_Tmp_Text$="[PgDn] "&Trim$(Gbw_Btn_Tmp_Text$)
					ELSE IF B_Key(Dev_J)=0 And Pos(Lwrc$(Gbw_Btn_Tmp_Text$),"enter")<=0 And Trim$(Gbw_Btn_Tmp_Text$)<>"" THEN 
						LET Gbw_Btn_Tmp_Text$="[Enter] "&Trim$(Gbw_Btn_Tmp_Text$)
					ELSE IF B_Key(Dev_J)=>1 And B_Key(Dev_J)<=12 And Pos(Lwrc$(Gbw_Btn_Tmp_Text$),"[f"&Str$(B_Key(Dev_J))&"]")<=0 THEN 
						LET Gbw_Btn_Tmp_Text$="[F"&Str$(B_Key(Dev_J))&"] "&Trim$(Gbw_Btn_Tmp_Text$)
					END IF  ! b_key(dev_j)=...
!         auto add fkey to label - end
				END IF  ! ~gbw_noadd_keys
				LET fnAddOneC(Mat Gbw_Btn_Text$,Gbw_Btn_Tmp_Text$)
				LET fnAddOneC(Mat Gbw_Btn_Fkey$,B_Key$(Dev_J))
			END IF  ! (mwb_button_row=1 and pos(b_lab$(dev_j),'(f)')>0) or (mwb_button_row=2 and pos(b_lab$(dev_j),'(f)')<=0)
		NEXT Dev_J
		LET Fn_Get_Button_Fields(Mat Gbw_Btn_Text$,Val(Gbw_Win_Part$(Pw_Cols)),Val(M_Row$),Mat Gbw_Btn_Fkey$,Mat Gbw_Field$)
		IF Gbw_Win_Button_Pop THEN 
			LET fnArrayAddC(Mat Gbw_Win_Button_Field$,Mat Gbw_Win_Button_Field$,Mat Gbw_Field$)
			LET fnArrayAddC(Mat Gbw_Win_Button_Text$,Mat Gbw_Win_Button_Text$,Mat Gbw_Btn_Text$)
		ELSE 
			PRINT #Gbw_H_Window,FIELDS Mat Gbw_Field$: Mat Gbw_Btn_Text$ IOERR GBW_PRINT_ERR
		END IF  ! gbw_win_button_pop   /   else 
		LET Auto_Add_Fkey_To_Label=0
	NEXT Mwb_Button_Row
	GOTO GBW_XIT
GBW_PRINT_ERR: ! 
	IF Developer THEN 
		FOR J=1 TO Udim(Mat Gbw_Field$) : PRINT 'gbw_field$('&Str$(J)&')='&Gbw_Field$(J)&' - gbw_btn_text$('&Str$(J)&')='&Gbw_Btn_Text$(J) : NEXT J
		PRINT 'could not print buttons - probably because the window is too narrow.'
		PAUSE 
	END IF  ! developer
	CONTINUE  ! GBW_PRINT_ERR
GBW_XIT: ! 
FNEND  ! fn_generate_buttons_for_window
DEF Fn_Get_Button_Fields(Mat Gbf_Label$,Gbf_Win_Width,Gbf_Row,Mat Gbf_Fkey$,Mat Gbf_Field$)
! todo: add button needs to auto move to correct position.  (far left i think)
! todo: add variable spacers between buttons,
! todo: right align buttons
	LET Gbf_Count=Udim(Mat Gbf_Label$)
	LET Gbf_Len_Max=0
	FOR Gbf_I=1 TO Gbf_Count
		LET Gbf_Label$(Gbf_I)=Trim$(Gbf_Label$(Gbf_I))
		LET Gbf_Len_Max=Max(Gbf_Len_Max,Len(Gbf_Label$(Gbf_I)))
	NEXT Gbf_I
	IF Gbf_Count>0 THEN 
		IF Gbf_Len_Max<=Int(Gbf_Win_Width/Gbf_Count) THEN 
			IF Gbf_Count<5 And Gbf_Len_Max*5<Gbf_Win_Width THEN 
				LET Gbf_Len_Button=Int(Gbf_Win_Width/5)
			ELSE 
				LET Gbf_Len_Button=Gbf_Len_Max
			END IF  ! gbf_count<5 and gbf_len_max*5<gbf_win_width   /   else 
		ELSE 
			LET Gbf_Len_Button=Int(Gbf_Win_Width/Gbf_Count)
		END IF 
	END IF  ! gbf_count>0 then
	MAT Gbf_Field$(Gbf_Count)
	FOR Gbf_I=1 TO Gbf_Count
		LET Gbf_Col=Gbf_Win_Width-(Gbf_Len_Button+1)+1 : LET Gbf_Win_Width-=(Gbf_Len_Button+1)
		IF Env$("GUIMODE")="ON" THEN
			Gbf_Field$(Gbf_I)=(Str$(Gbf_Row)&','&Str$(Gbf_Col)&','&Str$(Gbf_Len_Button)&'/Cc '&Str$(Gbf_Len_Max)&',[Button]S,B'&Gbf_Fkey$(Gbf_I))
		ELSE
			Gbf_Field$(Gbf_I)=(Str$(Gbf_Row)&','&Str$(Gbf_Col)&','&Str$(Gbf_Len_Button)&'/Cc '&Str$(Gbf_Len_Max)&',[Button_CUI]S,B'&Gbf_Fkey$(Gbf_I))
		end if
	NEXT Gbf_I
FNEND  ! fn_get_button_fields