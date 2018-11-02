

DEF Fn_Messagebox(Mbx_Message$*2048; Mbx_Type,Mbx_Title$*200,Mbx_Nogui$*28) ! mbx_
	LET Orig_Mb_Fkey=Fkey
! mbx_name$*20 - if included than a checkbox for "[ ] don't ask me again next time" will be added to the message
	IF ~Mbx_Setup THEN 
		LET Mbx_Setup=1
		DIM Mbx_Dont_Ask_Again_Text$*40
		DIM Mbx_Win_Data$(1)*128,Mbx_Win$*2048,Mbx_Name$*40,Mbx_Button_Fkey_List$*256
		DIM Mbx_File_Os$(1)*256
		DIM Mbx_File_V$(1)*256
		DIM Mbx_Win_Field$(1)*40,Mbx_Win_Text$(1)*40
!   let mbx_dont_ask_again_text$='Remember my answer next time.'
		LET Mbx_Wait_Time=15*60 ! if jbowman then let mbx_wait_time=3 else let mbx_wait_time=15*60
	END IF  ! ~mbx_setup
	IF Lwrc$(Env$('Messagebox_Always_Ask'))='yes' THEN LET Mbx_Always_Ask=1 ELSE LET Mbx_Always_Ask=0 ! note this line - this line forces the questions to be ask even if they have choosen not to remember prior answer.
	LET Mbx_Name$=Mbx_Nogui$
	LET Mbx_Message$=Srep$(Mbx_Message$,Crlf$,Lf$)
	LET Mbx_Message$=Srep$(Mbx_Message$,Cr$,Lf$)
	LET Mbx_Message$=Srep$(Mbx_Message$,'\n',Lf$)
	LET Fn_Mbx_Size(Mbx_Message$,Mb3_Text_Rows,Mb3_Cols)
	LET Mb3_Text_Rows=Max(Mb3_Text_Rows,5)
	LET Mb3_Parent_Rows=Mb3_Text_Rows+2
	LET Mb3_Cols=Min(Mb3_Cols,80)
	LET Mbx_Button_Default=Fn_Mbx_Button_Default(Mbx_Type)
	LET Mbx_Icon$=Fn_Mbx_Icon$(Mbx_Type)
	LET Mbx_Button_Count=Fn_Mbx_Button(Mbx_Type,Mbx_Button_Text$,Mat Mbx_Button_Return)
	IF Mbx_Name$<>'' THEN 
		LET Mb3_Parent_Rows+=1 ! add line for dont ask again text and a blank line
		IF Udim(Mat Mbx_Button_Return)=1 THEN 
			LET Mbx_Dont_Ask_Again_Text$='Do not notify me again.'
		ELSE 
			LET Mbx_Dont_Ask_Again_Text$='Remember my answer next time.'
		END IF 
		LET Mb3_Cols=Max(Mb3_Cols,Len(Mbx_Dont_Ask_Again_Text$)+10)
		LET Mbx_Dont_Ask_Row=Mb3_Parent_Rows-1 ! mbx_dont_ask_row is the row to print the dont ask again text on
	END IF  ! mbx_name$<>''
	LET Mbx_Button_Fkey_List$=''
	MAT Mbx_Button_Fkey(Mbx_Button_Count)
	FOR Mbx_Button_Fkey_Item=1 TO Mbx_Button_Count
		LET Mbx_Button_Fkey(Mbx_Button_Fkey_Item)=2010+Mbx_Button_Fkey_Item
		LET Mbx_Button_Fkey_List$(Inf:0)=Str$(Mbx_Button_Fkey(Mbx_Button_Fkey_Item))&';'
	NEXT Mbx_Button_Fkey_Item
	LET Mbx_Button_Fkey_List$=Mbx_Button_Fkey_List$(1:Len(Mbx_Button_Fkey_List$)-1) ! remove the last ;
! 
	IF Mbx_Type<>0 THEN PRINT 'mbx_type<>0 - something must be wrong' : PAUSE 
! 
	IF Mbx_Name$<>'' THEN 
		LET Mbx_Name$='mbx_'&Trim$(Mbx_Name$)&'_'&Str$(Mb3_Parent_Rows)&'x'&Str$(Mb3_Cols)
		LET Mbx_Win$='name='&Mbx_Name$&','
		LET Mbx_Checked=Fnregunum_Get(Mbx_Name$)
	ELSE 
		LET Mbx_Checked=0
	END IF  ! mbx_name$<>''   /   else 
	LET Mbx_File_Count=Fn_Mbx_Get_File_List(Mbx_H_Text,Mbx_Message$,Mbx_H_Cols,Mat Mbx_File_Os$,Mat Mbx_File_V$)
	IF Mbx_Always_Ask Or Mbx_Checked<=9999 THEN 
		MAT Mbx_Win_Field$(1)=('')
		MAT Mbx_Win_Text$(1)=('')
		LET Mbx_Win$=''
		LET Mbx_Timeout=0
!   if mbx_checked>9999 then let mbx_dont_ask_again_text$='^'&mbx_dont_ask_again_text$
		LET Mbx_Win$(Inf:0)='Caption='&Srep$(Mbx_Title$,',','.')
		LET Mbx_Win$(Inf:0)=',rows='&Str$(Mb3_Parent_Rows)
		LET Mbx_Win$(Inf:0)=',cols='&Str$(Mb3_Cols)
		LET Mbx_Win$(Inf:0)=',center'
		LET Mbx_Win$(Inf:0)=',button.text='&Mbx_Button_Text$ ! &';(f)Copy'
		IF Mbx_File_Count>0 THEN 
			LET Mbx_Win$(Inf:0)=';(f)File'
			IF Mbx_File_Count>1 THEN LET Mbx_Win$(Inf:0)='s'
!     mbx_button_default+=1 ! because File will be the first button, so skip it.
		END IF  ! mbx_file_count>0
		LET Mbx_Win$(Inf:0)=',button.fkey='&Mbx_Button_Fkey_List$ ! &';33'
		IF Mbx_File_Count>0 THEN 
			LET Mbx_Win$(Inf:0)=';33'
		END IF  ! mbx_file_count>0
		LET Mbx_Win$(Inf:0)=',parent=none'
		IF ~Developer THEN LET Mbx_Win$(Inf:0)=',modal,Nomaximize'
		OPEN #Mbx_H_Win:=Fngethandle_: Fn_Open_Parent$(Mbx_Win$,Mat Mbx_Win_Data$,1,1),DISPLAY,OUTPUT 
		LET Fn_Generate_Buttons_For_Window(Mat Mbx_Win_Data$,Mbx_H_Win, 1,Mat Mbx_Win_Field$,Mat Mbx_Win_Text$)
		LET Fn_Array_Reverse$(Mat Mbx_Win_Field$)
		LET Fn_Array_Reverse$(Mat Mbx_Win_Text$)
		IF Exists('Icons\'&Mbx_Icon$&'.png') THEN PRINT #Mbx_H_Win,FIELDS '1,1,P 4/8,[W]': 'Icons\'&Mbx_Icon$&'.png:isotropic'
		LET Mbx_H_Cols=Val(Mbx_Win_Data$(Pw_Cols))
		LET Mbx_H_Text_Rows=Mb3_Text_Rows
		LET Mbx_H_Text_Cols=Mbx_H_Cols-10
		OPEN #Mbx_H_Text:=Fngethandle_: 'Modal,Parent='&Str$(Mbx_H_Win)&',Border=None,SRow=1,SCol=10,Rows='&Str$(Mbx_H_Text_Rows)&',Cols='&Str$(Mbx_H_Text_Cols),DISPLAY,OUTPUT 
! if jbowman then print 'mbx_message$='&mbx_message$ : pause
		PRINT #Mbx_H_Text,FIELDS '1,1,'&Fn_Mbx_Text_Size$(Mbx_H_Text_Cols,Mbx_H_Text_Rows)&',[T]S,49': Mbx_Message$
		IF Mbx_Name$<>'' THEN 
			LET Fn_Add_One$(Mat Mbx_Win_Field$,Str$(Mbx_Dont_Ask_Row)&',10,Check '&Str$(Len(Mbx_Dont_Ask_Again_Text$)+1)&',[W]ae,2020')
			LET Fn_Add_One$(Mat Mbx_Win_Text$,Rpt$('^',Min(1,Mbx_Checked-10000))&Mbx_Dont_Ask_Again_Text$)
			LET Mbx_Win_Which_Don_Ask=Udim(Mat Mbx_Win_Text$)
			IF Mbx_Checked-10000>0 THEN LET Mbx_Button_Default=Srch(Mat Mbx_Button_Fkey,Mbx_Checked-10000)
		END IF  ! mbx_name$<>''
		FOR Mbx_Win_Field_Item=1 TO Udim(Mat Mbx_Win_Field$)
			LET Mbx_Win_Field$(Mbx_Win_Field_Item)=Srep$(Mbx_Win_Field$(Mbx_Win_Field_Item),'[Button]','[Button]ae')
		NEXT Mbx_Win_Field_Item
		LET Curfld(Mbx_Button_Default)
		DO 
			LET Mb_X_Click=0
			RINPUT #Mbx_H_Win,SELECT Mat Mbx_Win_Field$,ATTR '[A]',WAIT=Mbx_Wait_Time: Mat Mbx_Win_Text$ TIMEOUT IGNORE
			IF Fkey=93 Or Fkey=99 THEN 
				GOSUB MBX_DEFAULT_FKEY 
				LET Mb_X_Click=Mb_X
			ELSE IF Fkey=101 THEN 
				LET Mbx_Timeout=1
				GOSUB MBX_DEFAULT_FKEY
			ELSE IF Fkey=2020 THEN 
				IF Mbx_Win_Text$(Mbx_Win_Which_Don_Ask)(1:1)='^' THEN 
					LET Mbx_Win_Text$(Mbx_Win_Which_Don_Ask)(1:1)=''
				ELSE 
					LET Mbx_Win_Text$(Mbx_Win_Which_Don_Ask)(0:0)='^'
				END IF  ! mbx_win_text$(mbx_win_which_don_ask)(1:1)='^'   /   else 
			ELSE IF Fkey=33 And Mbx_File_Count>0 THEN ! Alt+F
				LET Fn_Mbx_File_Menu(Mbx_Title$,Mbx_File_Count,Mbx_H_Text_Cols,Mat Mbx_File_Os$,Mat Mbx_File_V$,Mbx_Wait_Time)
			ELSE IF Fkey=46 THEN ! Alt+C
				LET Setenv('ClipBoard',Srep$(Mbx_Message$,Lf$,Crlf$))
			ELSE IF Fkey=99 And Mbx_Button_Count=1 THEN 
				LET Fkey(Mbx_Button_Fkey(1))
			END IF  ! fkey=...
		LOOP Until Srch(Mat Mbx_Button_Fkey,Fkey)>0
		IF Mbx_Name$<>'' And Fkey<>101 THEN 
			IF Mbx_Win_Text$(Mbx_Win_Which_Don_Ask)(1:1)='^' THEN LET Mbx_Checked=1 ELSE LET Mbx_Checked=0
			IF Mbx_Checked THEN 
				LET Fnregunum_Put(Mbx_Name$,10000+Fkey)
			ELSE 
				LET Fnregunum_Put(Mbx_Name$,0)
			END IF  ! mbx_checked   /   else 
		END IF  ! mbx_name$=''
		CLOSE #Mbx_H_Win: 
	ELSE 
!   print 'returning with fkey '&str$(mbx_checked-10000)&' from memory'
		LET Fkey(Mbx_Checked-10000)
!   pause
	END IF  ! mbx_always_ask or mbx_checked<=9999   /   else 
	GOTO MBX_XIT
MBX_DEFAULT_FKEY: ! 
	IF Curfld<=Mbx_Button_Count And Srch(Mat Mbx_Button_Fkey,Mbx_Button_Fkey(Curfld))>0 THEN 
		LET Fkey(Mbx_Button_Fkey(Curfld))
	ELSE 
		LET Fkey(Mbx_Button_Fkey(Mbx_Button_Default))
	END IF  ! curfld<=mbx_button_count and srch(mat mbx_button_fkey,mbx_button_fkey(curfld))>0   /   else 
	RETURN  ! MBX_DEFAULT_FKEY
MBX_XIT: ! 
	IF Mb_X_Click THEN 
		LET Fn_Messagebox=Mb_X_Click 
	ELSE 
		LET Fn_Messagebox=Mbx_Button_Return(Srch(Mat Mbx_Button_Fkey,Fkey))
	end if
	IF Mbx_Timeout THEN LET Fkey(101)
	IF Fkey<>93 And Fkey<>101 THEN LET Fkey(Orig_Mb_Fkey)
	LET Orig_Mb_Fkey=0
FNEND  ! fn_messagebox
DEF Fn_Mbx_File_Menu(Mbxfm_Title$*80,Mbxfm_File_Count,Mbxfm_H_Text_Cols,Mat Mbxfm_File_Os$,Mat Mbxfm_File_V$,Mbxfm_Wait_Time) ! mbxfm_
! 
	DIM Mbxfm_Action_Win$*256,Mbxfm_Win_Action_Data$(1)*128
	DIM Mbxfm_File_Path$*256,Mbxfm_File_Name_Base$*128,Mbxfm_File_Name_Extension$*128
	DIM Mbxfm_H_Tab(2)
	DIM Mbxfm_Action_Tab$*256,Mbxfm_Selection$*256
	DIM Mbxfm_File_Field$(1)*20
	LET Mbxfm_Action_Win$=Mbxfm_S$=''
	IF Mbxfm_File_Count>1 THEN LET Mbxfm_S$='s'
	LET Mbxfm_Action_Win$(Inf:0)='Caption='&Mbxfm_Title$&' - File'&Mbxfm_S$
	LET Mbxfm_Action_Win$(Inf:0)=',center'
	LET Mbxfm_Action_Win$(Inf:0)=',rows='&Str$(Mbxfm_File_Count+3)
	LET Mbxfm_Action_Win$(Inf:0)=',cols='&Str$(Mbxfm_Win_Action_Cols:=Max(40,Mbxfm_H_Text_Cols)+2)
	LET Mbxfm_Action_Win$(Inf:0)=',button.text=Close;(f)Copy;(f)Open;(f)Open Folder'
	LET Mbxfm_Action_Win$(Inf:0)=',button.fkey=99;2001;2002;2003'
	LET Mbxfm_Action_Win$(Inf:0)=',parent=none'
	IF ~Developer THEN LET Mbxfm_Action_Win$(Inf:0)=',modal,Nomaximize'
	OPEN #Mbxfm_H_Win:=Fngethandle_: Fn_Open_Parent$(Mbxfm_Action_Win$,Mat Mbxfm_Win_Action_Data$,1),DISPLAY,OUTPUT 
	LET Mbxfm_Action_Tab$='SRow=2,SCol=2,Rows='&Str$(Mbxfm_File_Count+0)&',Cols='&Str$(Mbxfm_Win_Action_Cols-2)&',Parent='&Str$(Mbxfm_H_Win)
	LET Fn_Generate_Buttons_For_Window(Mat Mbxfm_Win_Action_Data$,Mbxfm_H_Win)
	OPEN #Mbxfm_H_Tab(1):=Fngethandle_: Fn_Open_Parent$(Mbxfm_Action_Tab$&',Tab=OS Path'&Mbxfm_S$,Mat Mbxfm_Tab_Data$,1),DISPLAY,OUTPUT 
	OPEN #Mbxfm_H_Tab(2):=Fngethandle_: Fn_Open_Parent$(Mbxfm_Action_Tab$&',Tab=Virtual Path'&Mbxfm_S$,Mat Mbxfm_Tab_Data$,1),DISPLAY,OUTPUT 
	MAT Mbxfm_File_Field$(Mbxfm_File_Count)
	FOR Mbxfm_File_Item=1 TO Mbxfm_File_Count
		LET Mbxfm_File_Field$(Mbxfm_File_Item)=Str$(Mbxfm_File_Item)&',1,40/C '&Mbxfm_Win_Action_Data$(Pw_Cols)&',[M]'
	NEXT Mbxfm_File_Item
	LET Mbxfm_Curtab=Fnregunum_Get('mbxfm_CurTab')
	IF Mbxfm_Curtab<=0 THEN LET Mbxfm_Curtab=2
	DO 
		LET Curfld(Mbxfm_Curfld)
		IF Mbxfm_Curtab=1 THEN 
			RINPUT #Mbxfm_H_Tab(Mbxfm_Curtab),SELECT Mat Mbxfm_File_Field$, ATTR '[L]',WAIT=Mbxfm_Wait_Time: Mat Mbxfm_File_Os$ TIMEOUT IGNORE
			LET Mbxfm_Selection$=Mbxfm_File_Os$(Curfld)
		ELSE ! if mbxfm_curtab=2 then
			RINPUT #Mbxfm_H_Tab(Mbxfm_Curtab),SELECT Mat Mbxfm_File_Field$, ATTR '[L]',WAIT=Mbxfm_Wait_Time: Mat Mbxfm_File_V$ TIMEOUT IGNORE
			LET Mbxfm_Selection$=Mbxfm_File_V$(Curfld)
		END IF  ! mbxfm_curtab=1   /   else 
		LET Mbxfm_Curfld=Curfld
		IF Fkey=92 THEN 
			IF Mbxfm_Curtab=1 THEN LET Mbxfm_Curtab=2 ELSE LET Mbxfm_Curtab=1
		ELSE IF Fkey=101 THEN 
			LET Fkey(99)
		ELSE IF Fkey=2001 THEN 
			LET Setenv('ClipBoard',Mbxfm_Selection$)
		ELSE IF Fkey=2002 THEN 
			LET Fn_Exe('-c -w','VB32\WinShell',Mbxfm_File_Os$(Curfld))
		ELSE IF Fkey=2003 THEN 
			LET Fn_Filepath_Parse(Mbxfm_File_Os$(Curfld),Mbxfm_File_Path$,Mbxfm_File_Name_Base$,Mbxfm_File_Name_Extension$)
			EXECUTE "System -c -w explorer /select,"&Srep$(Mbxfm_File_Os$(Curfld),"@::","")&'"'             ! fn_exe MIGHT NOT BE A GOOD IDEA HERE!
		END IF  ! fkey=92   /   101   /   2001   /   2002   /   2003
	LOOP Until Fkey=99 Or Fkey=93
	LET Fnregunum_Put('mbxfm_CurTab',Mbxfm_Curtab)
	CLOSE #Mbxfm_H_Win: 
FNEND  ! fn_mbx_file_menu
DEF Fn_Mbx_Get_File_List(Mbxht_H_Parent,Mbxht_Text$*2048,Mbxht_Cols,Mat Mbxht_File_Os$,Mat Mbxht_File_V$) ! mbxht_
	DIM Mbxht_Line$(1)*1024
	LET Str2mat(Mbxht_Text$,Mat Mbxht_Line$) ! No Paramater needed here LF of all types
	MAT Mbxht_File_Os$(0)
	MAT Mbxht_File_V$(0)
	FOR Mbxht_Line_Item=1 TO Udim(Mat Mbxht_Line$)
		IF Mbxht_Line$(Mbxht_Line_Item)(1:2)='\\' Or Mbxht_Line$(Mbxht_Line_Item)(2:2)=':' THEN ! the line specifies a file
			LET Os_Filename$(Mbxht_Line$(Mbxht_Line_Item)) ERROR 30260 ! Check for "Bad File Names"
			LET Fn_Add_One$(Mat Mbxht_File_V$,Mbxht_Line$(Mbxht_Line_Item),0,1)
			LET Fn_Add_One$(Mat Mbxht_File_Os$,Fn_Client_Os_Path$(Mbxht_Line$(Mbxht_Line_Item)),0,1) ! Fn_Os_Filename$ - FN_Client_OS_Path does this!
		END IF  ! mbxht_line$(mbxht_line_item)(1:2)='\\' or mbxht_line$(mbxht_line_item)(2:2)=':'
	NEXT Mbxht_Line_Item
	LET Fn_Mbx_Get_File_List=Udim(Mat Mbxht_File_Os$)
FNEND  ! fn_mbx_get_file_list
DEF Fn_Mbx_Text_Size$(Mts_Cols,Mts_Rows)
	LET Mts_Cols_Original=Mts_Cols
	LET Mts_Rows_Original=Mts_Rows
	LET Fn_Mbx_Text_Size$=Str$(Mbx_H_Text_Cols*Mbx_H_Text_Rows)&"/C" ! str$(mts_cols_original*mts_rows_original)&'/Cl '&str$(mts_cols*mts_rows)
FNEND  ! fn_mbx_text_size$
DEF Fn_Mbx_Size(&Mbxs_Message$,&Mbxs_Rows,&Mbxs_Cols) ! mbxs_
	DIM Mbxs_Line$(1)*1024
	LET Str2mat(Mbxs_Message$,Mat Mbxs_Line$) ! ,LF$ is not needed because STR2MAT checks all types or LF
	LET Mbxs_Rows=Udim(Mat Mbxs_Line$)
	LET Mbxs_Cols=0
	FOR Mbxs_Line_Item=1 TO Mbxs_Rows
		LET Mbxs_Cols=Max(Mbxs_Cols,Len(Mbxs_Line$(Mbxs_Line_Item)))
		LET Mbxs_Cols+=Fn_Chr_Count(Mbxs_Line$(Mbxs_Line_Item),Tab$)*4
	NEXT Mbxs_Line_Item
	IF Mbxs_Cols>140 THEN 
		LET Mbxs_Cols=Ceil(Mbxs_Cols*.78)
	ELSE IF Mbxs_Cols>90 THEN 
		LET Mbxs_Cols=Ceil(Mbxs_Cols*.82)
	ELSE IF Mbxs_Cols>60 THEN 
		LET Mbxs_Cols=Ceil(Mbxs_Cols*.86)
	ELSE IF Mbxs_Cols>30 THEN 
		LET Mbxs_Cols=Ceil(Mbxs_Cols*.90)
! else 
!   let mbxs_cols=ceil(mbxs_cols*1.0)
	END IF 
	LET Mbxs_Cols=Max(Mbxs_Cols,10)
	LET Mbxs_Cols+=13
FNEND  ! fn_mbx_size
DEF Fn_Mbx_Button_Default(&Mbx_Type)
	IF Mbx_Type>=Mb_Button3_Default THEN 
		LET Mbx_Type-=Mb_Button3_Default
		LET Mbx_Button_Default=3
	ELSE IF Mbx_Type>=Mb_Button2_Default THEN 
		LET Mbx_Type-=Mb_Button2_Default
		LET Mbx_Button_Default=2
	ELSE 
		LET Mbx_Button_Default=1
	END IF  ! mbx_type
	LET Fn_Mbx_Button_Default=Mbx_Button_Default
FNEND  ! fn_mbx_button_default
DEF Fn_Mbx_Icon$(&Mbx_Type)
	IF Mbx_Type>=Mb_Information THEN 
		LET Mbx_Type-=Mb_Information
		LET Mbx_Icon$='information'
	ELSE IF Mbx_Type>=Mb_Exclamation THEN 
		LET Mbx_Type-=Mb_Exclamation
		LET Mbx_Icon$='exclamation'
	ELSE IF Mbx_Type>=Mb_Question THEN 
		LET Mbx_Type-=Mb_Question
		LET Mbx_Icon$='question'
	ELSE IF Mbx_Type>=Mb_Stop THEN 
		LET Mbx_Type-=Mb_Stop
		LET Mbx_Icon$='stop'
	ELSE 
!   if developer or debug then print bell;'no icon selected for messagegox - default of "information" will be used.'
!   if developer then pause
		LET Mbx_Icon$='information'
	END IF 
	LET Fn_Mbx_Icon$=Mbx_Icon$
FNEND  ! fn_mbx_icon$
DEF Fn_Mbx_Button(&Mbxb_Type,&Mbxb_Button_Text$,Mat Mbxb_Button_Return) ! mbxb_
	MAT Mbxb_Button_Return(1) : LET Mbxb_Button_Text$="?"&Str$(Mbxb_Type): LET Mbxb_Button_Return(1)=Mb_Ok
	LET Mbxb_Button_Text$=''
	IF Mbxb_Type>=Mb_Retrycancel THEN 
		LET Mbxb_Type-=Mb_Retrycancel : LET Mbxb_Button_Text$='Retry;Cancel' 
		MAT Mbxb_Button_Return(2) : LET Mbxb_Button_Return(1)=Mb_Retry 
		LET Mbxb_Button_Return(2)=Mb_Cancel : LET Mb_X=Mb_Cancel
	ELSE IF Mbxb_Type>=Mb_Yesno THEN 
		LET Mbxb_Type-=Mb_Yesno
		LET Mbxb_Button_Text$='Yes;No'
		MAT Mbxb_Button_Return(2) : LET Mbxb_Button_Return(1)=Mb_Yes 
		LET Mbxb_Button_Return(2)=Mb_No : LET Mb_X=Mb_No
	ELSE IF Mbxb_Type>=Mb_Yesnocancel THEN 
		LET Mbxb_Type-=Mb_Yesnocancel
		LET Mbxb_Button_Text$='Yes;No;Cancel'
		MAT Mbxb_Button_Return(3) : LET Mbxb_Button_Return(1)=Mb_Yes 
		LET Mbxb_Button_Return(2)=Mb_No : LET Mbxb_Button_Return(3)=Mb_Cancel 
		LET Mb_X=Mb_Cancel
	ELSE IF Mbxb_Type>=Mb_Abortretryignore THEN 
		LET Mbxb_Type-=Mb_Abortretryignore
		LET Mbxb_Button_Text$='Abort;Retry;Ignore'
		MAT Mbxb_Button_Return(3) : LET Mbxb_Button_Return(1)=Mb_Abort 
		LET Mbxb_Button_Return(2)=Mb_Retry : LET Mbxb_Button_Return(3)=Mb_Ignore 
		LET Mb_X=Mb_Abort
	ELSE IF Mbxb_Type>=Mb_Okcancel THEN 
		LET Mbxb_Type-=Mb_Okcancel
		LET Mbxb_Button_Text$='OK;Cancel'
		MAT Mbxb_Button_Return(2) : LET Mbxb_Button_Return(1)=Mb_Ok 
		LET Mbxb_Button_Return(2)=Mb_Cancel : LET Mb_X=Mb_Cancel
	ELSE 
		MAT Mbxb_Button_Return(1) : LET Mbxb_Button_Return(1)=Mb_Ok
		LET Mbxb_Type-=Mb_Okonly
		LET Mbxb_Button_Text$='OK' 
		LET Mb_X=Mb_Ok
	END IF 
	LET Fn_Mbx_Button=Udim(Mat Mbxb_Button_Return)
FNEND  ! fn_mbx_button



DEF Fn_Exe(Exe_Parameters$*32,Exe_Name$*256;Exe_Filename$*256,Exe_Add_Parameters$*2048)
	LET Exe_Name$=Srep$(Exe_Name$,"[PKZIP]",Env$("Client_Zip_Program")) 
	LET Exe_Name$=Srep$(Exe_Name$,"[PKUNZIP]",Env$("Client_UnZip_Program"))
	LET Exe_Quote$='"' 
	IF Pos(Exe_Parameters$,"QUOTE=NONE")>0 Or Uprc$(Trim$(Exe_Name$))(1:5)="START" THEN 
		LET Exe_Quote$='' 
		LET Exe_Parameters$=Srep$(Exe_Parameters$,"QUOTE=NONE","") 
		LET Exe_Parameters$=Trim$(Exe_Parameters$)
	end if
	IF Uprc$(Trim$(Exe_Name$))(1:5)<>"START" THEN 
		LET Exe_Name$=Fn_Client_Os_Path$(Exe_Name$)
	end if
	IF Trim$(Exe_Filename$)<>"" THEN 
		LET Exe_Filename$=Fn_Client_Os_Path$(Exe_Filename$)
	end if
	LET Exe_Filename$=Srep$(Exe_Filename$,"@::","") 
	LET Exe_Name$=Srep$(Exe_Name$,"@::","")
	IF Trim$(Exe_Filename$)<>"" And Exe_Filename$(1:1)<>Exe_Quote$ And Exe_Quote$<>"" THEN 
		LET Exe_Filename$=Exe_Quote$&Trim$(Exe_Filename$)&Exe_Quote$
	end if
	IF Pos(Exe_Name$," ")>0 THEN 
		EXECUTE "SYS "&Exe_Parameters$&' "'&Trim$(Trim$(Exe_Name$)&'" '&Trim$(Exe_Filename$))&" "&Trim$(Exe_Add_Parameters$) 
	ELSE 
		EXECUTE "SYS "&Exe_Parameters$&' '&Trim$(Trim$(Exe_Name$)&' '&Trim$(Exe_Filename$))&" "&Trim$(Exe_Add_Parameters$)
	end if
FNEND 

