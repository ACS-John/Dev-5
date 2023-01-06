! r: testing zone
!    fn_setup
!   fnTop('W-3 Line Testing')
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
		autoLibrary
		fnreg_read('W-3 - Enable Background' 	,enableBackground$ 	,'True' )
		MarginTopN =fnReg_read('W-3 - Margin Top'        	,MarginTop$        	,'5' )
		MarginLeftN=fnReg_read('W-3 - Margin Left'       	,MarginLeft$       	,'7' )
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
	if ~setup then fn_setup
	fnpa_open('','W-3','PDF')
	if enableBackground$='True' then fnpa_background('S:\Core\pdf\'&taxyear$&'\W-3.pdf')
	fnpa_fontsize(12)
	fnpa_txt('X',37,fn_line(2))
	fnpa_txt(' ',37,fn_line(2))
	col1=  1+MarginLeftN
	! if env$('client')='Kincaid' then col1+=2
	col2=113-7+MarginLeftN
	col3=169-7+MarginLeftN
	 ! if env$('acsdeveloper')<>'' then  pause ! specialform2018=1
	! removed 1/21/2022        ! if env$('client')='Thomasboro' or env$('client')='Cerro Gordo V' or env$('client')='Cerro Gordo T' or env$('client')='Kincaid' or env$('client')='Hope Welty' or env$('client')='Bethany' then specialform2018=1
	fnpa_txt(ein$,col1,fn_line(5)) ! Employer Identification Number (EIN)
	fnpa_txt(a$(1),col1,fn_line(6))
	fnpa_txt(a$(2),col1,fn_line(7)-2)
	fnpa_txt(a$(3),col1,fn_line(7)+2)
	fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',w(2) ),col2,fn_line(4)) ! taxable wages
	fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',w(1) ),col3,fn_line(4)) ! fed wh
	fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',w(5) ),col2,fn_line(5)) ! ss wages
	fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',w(3) ),col3,fn_line(5)) ! ss wh
	fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',w(11)),col2,fn_line(6)) ! mc wages
	fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',w(12)),col3,fn_line(6)) ! medicare wh
	fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',w(6) ),col2,fn_line(7)) ! ss-tips
	fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',w(4) ),col3,fn_line(8)) ! eic
	fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',dcb  ),col3,fn_line(9)) ! deferred comp
	fnpa_txt(state$,12,fn_line(11))
	fnpa_txt(stcode$,22,fn_line(11))
	col1=  8
	col2= 51
	col3=110
	col4=169 
	fnpa_txt(cnvrt$('pic(zzzzzzzzzzzzzz.zz',w(9)),col1,fn_line(12)) ! state wages
	fnpa_txt(cnvrt$('pic(zzzzzzzzzzzzzz.zz',w(7)),col2,fn_line(12)) ! state wh
	fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',w(10)),col3,fn_line(12)) ! local wages
	fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',w(8)),col4,fn_line(12)) ! local wh
	fnpa_finis
fnend
def fn_line(lineNumber; ___,returnN)
	if lineNumber=1 then 
		returnN=10
	else  ! if lineNumber>=1 and lineNumber<=14 then
		returnN=20+(8.5*(lineNumber-2))
	end if 
	if specialform2018=1 then 
		returnN-=1
		if linenumber<10 then
			returnN-=2
		end if 
	end if 
	returnN-=5
	fn_line=returnN+MarginTopN
fnend


