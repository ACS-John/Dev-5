	! r: testing zone
	!    fn_setup
	!   fntop('W-3 Line Testing')
	!   fnpa_open('','W-3','PDF')
	!   fnpa_background('S:\Core\pdf\W-3.pdf')
	!   for lyne=1 to 14
	!     fnpa_txt(str$(lyne)&'. XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX' ,8,fn_line(lyne))
	!   nex lyne
	!   fnpa_finis
	!   pr 'done.' : end
	! /r
	def fn_setup
		if ~setup then
			setup=1
			library 'S:\Core\Library': fntop,fnxit,fnchain,fnTos,fnFra,fnLbl,fnTxt,fnCmdKey,fnAcs,fnOpt,fnmsgbox,fnChk,fnpa_finis,fnerror,fnureg_read,fnureg_write,fnButton,fnCmdSet,fnpa_open,fnpa_newpage,fnpa_fontsize,fnpa_txt,fncreg_read,fncreg_write,fnpa_background,fngethandle,fnDedNames,fnreg_read,fnreg_write,fncomboa
			fnreg_read('W-3 - Enable Background'              ,enableBackground$   ,'True' )
		end if
	fnend
	def library fnw3(taxyear$,ein$*12,mat a$,mat w,dcb,state$,stcode$;___,specialform2018)
		! ein$      Employer Identification Number (EIN)
		! a$(1)     Employer Name
		! a$(2)     Employer Address
		! a$(3)     Employer City, State, Zip
		! w(1)      fed wh
		! w(2)      taxable wages
		! w(3)      ss wh
		! w(4)      eic
		! w(5)      ss wages
		! w(6)      ss-tips
		! w(7)      state wh
		! w(8)      local wh
		! w(9)      state wages
		! w(10)     local wages
		! w(11)     mc wages
		! w(12)     mc medicare
		! dcb       deferred comp
		! state$    
		! stcode$   
		if ~setup then let fn_setup
		fnpa_open('','W-3','PDF')
		if enableBackground$='True' then let fnpa_background('S:\Core\pdf\'&taxyear$&'\W-3.pdf')
		fnpa_fontsize(12)
		fnpa_txt("X",38,fn_line(2))
		col1=  8
		col2=113
		col3=169
		 ! if env$('acsdeveloper')<>'' then  pause ! let specialform2018=1
		if env$('client')='Thomasboro' or env$('client')="Cerro Gordo" or env$('client')="Cerro Gordo T" or env$('client')="Kincaid" or env$('client')="Hope Welty" or env$('client')="Bethany" then let specialform2018=1
		fnpa_txt(ein$,col1,fn_line(5)) ! Employer Identification Number (EIN)
		fnpa_txt(a$(1),col1,fn_line(6))
		fnpa_txt(a$(2),col1,fn_line(7)-2)
		fnpa_txt(a$(3),col1,fn_line(7)+2)
		fnpa_txt(cnvrt$("pic(--,---,---.##",w(2) ),col2,fn_line(4)) ! taxable wages
		fnpa_txt(cnvrt$("pic(--,---,---.##",w(1) ),col3,fn_line(4)) ! fed wh
		fnpa_txt(cnvrt$("pic(--,---,---.##",w(5) ),col2,fn_line(5)) ! ss wages
		fnpa_txt(cnvrt$("pic(--,---,---.##",w(3) ),col3,fn_line(5)) ! ss wh
		fnpa_txt(cnvrt$("pic(--,---,---.##",w(11)),col2,fn_line(6)) ! mc wages
		fnpa_txt(cnvrt$("pic(--,---,---.##",w(12)),col3,fn_line(6)) ! medicare wh
		fnpa_txt(cnvrt$("pic(--,---,---.##",w(6) ),col2,fn_line(7)) ! ss-tips
		fnpa_txt(cnvrt$("pic(--,---,---.##",w(4) ),col3,fn_line(8)) ! eic
		fnpa_txt(cnvrt$("pic(--,---,---.##",dcb  ),col3,fn_line(9)) ! deferred comp
		fnpa_txt(state$,12,fn_line(11))
		fnpa_txt(stcode$,22,fn_line(11))
		col1=  8
		col2= 51
		col3=110
		col4=169 
		fnpa_txt(cnvrt$("pic(--,---,---,zzz.##",w(9)),col1,fn_line(12)) ! state wages
		fnpa_txt(cnvrt$("pic(--,---,---,zzz.##",w(7)),col2,fn_line(12)) ! state wh
		fnpa_txt(cnvrt$("pic(--,---,---.##",w(10)),col3,fn_line(12)) ! local wages
		fnpa_txt(cnvrt$("pic(--,---,---.##",w(8)),col4,fn_line(12)) ! local wh
		fnpa_finis
	fnend
	def fn_line(lineNumber)
		lReturn=0
		if lineNumber=1 then 
			lReturn=10
		else  ! if lineNumber>=1 and lineNumber<=14 then
			lReturn=20+(8.5*(lineNumber-2))
		! else 
		!   pr 'invalid lineNumber ('&str$(lineNumber)&')'
		!   pause
		end if 
		if specialform2018=1 then 
			let lreturn-=1
			if linenumber<10 then
				let lreturn-=2
			end if 
		end if 
		fn_line=lReturn
	fnend


