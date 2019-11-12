SetupPrint: ! r:
	if ~setupPrint then
		setupPrint=1
		library "Print/Library": fnSel_lib,fnGet_prnLegacyVar,fnGet_prnVar$,fnclose,fnpage
		library "Print/Library": fnReopen_last_printer
		! library "Print/Library": fnGet_prnLegacyVar,fnGet_prnVar$,fnclose,fnpage
		dim prn_data$(1)*80,prn_fieldsc$(1)*20 ! for fnGet_prnVar$
		! dim prnname$*80,comp$*80,orient$(6)*30,size$(18)*30,tsize$(3)*80,boldon$*18,boldoff$*18,undon$*18,undoff$*18 ! for fnGet_prnLegacyVar
	end if
return
! Updateable Region . Printer Selection . Top 
! This region was last updated on 2019.01.10
def fnSel(width; printer_prompt$*80,printfile_handle, print_cancel_option$*80,supported_printer_type_list$*80,print_destination_custom$*1024,print_pk$*32) ! open printer routine
	if ~setupPrint then gosub SetupPrint
	on soflow ignore
	fnSel=fnSel_lib(Width,Printer_Prompt$,Printfile_Handle, Print_Cancel_Option$,Supported_Printer_Type_List$,Print_Destination_Custom$,Pk$)
	if fkey=93 or fkey=99 then
		prnname$=comp$=ff$=orient$=""
		lpp=wid=prn_num=0
		mat prn_data$=("") : mat orient$=("") : mat size$=("") : mat tsize$=("")
	else 
		execute fnGet_prnVar$(Mat Prn_Data$,Mat Prn_Fieldsc$)
		! fnGet_prnLegacyVar(prnname$, mat orient$, mat size$, mat tsize$, comp$, ff$, lpp, wid, orient$, prn_num,boldon$,boldoff$,undon$,undoff$)
	end iF  ! FKey=93 or FKey=99   /   else 
	on soflow system
fnend
FORM_FNSEL: Form pos 1,C ,Skip 0
! Updateable Region . Printer Selection . End 
! ________________________________________________________________________