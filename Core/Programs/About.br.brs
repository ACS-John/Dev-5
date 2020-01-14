library 'S:\Core\Library': fntop,fnxit,fnclient_has_mat,fnSystemNameFromAbbr$,fnmsgbox,fnAcs,fnLbl,fnTxt,fngethandle,fnTos,fnerror,fncno,fnCmdSet,fnChk,fncd,fnactpd,fnStatus,fnqgl,fnagl$,fnindex_it,fnrgl$,fnclient_support
on error goto Ertn
dim txt$(1)*256
fntop(program$, cap$="About ACS")
c_has_count=fnclient_has_mat(mat c_has$)
fnclient_support(mat system_id$,mat system_support_end_date,mat on_support)
txt_item=0
mat txt$(txt_item+=1) : txt$(txt_item)='ACS Version '&env$('acsVersion')
mat txt$(txt_item+=1) : txt$(txt_item)=chr$(9)&'Customized for '&env$('Client')
if env$('user_limit')='1' then 
	mat txt$(txt_item+=1) : txt$(txt_item)=chr$(9)&env$('user_limit')&' User Liscensed'
else 
	mat txt$(txt_item+=1) : txt$(txt_item)=chr$(9)&env$('user_limit')&' Users Liscensed'
end if 
mat txt$(txt_item+=1) : txt$(txt_item)='Licensed Systems:'
for c_has_item=1 to c_has_count
	mat txt$(txt_item+=1) : txt$(txt_item)
	txt$(txt_item)=chr$(9)&rpad$(fnSystemNameFromAbbr$(c_has$(c_has_item)),40)
	! r: add support information
	which=srch(mat system_id$,c_has$(c_has_item))
	mat txt$(txt_item+=1) : txt$(txt_item)
	if which>0 then 
		if days(date('ccyymmdd'),'ccyymmdd')<=days(system_support_end_date(which),'ccyymmdd') then 
			txt$(txt_item)=chr$(9)&chr$(9)&'Support active until '
		else 
			txt$(txt_item)=chr$(9)&chr$(9)&'Support expired on '
		end if 
		txt$(txt_item)&=cnvrt$('pic(####/##/##)',system_support_end_date(which))
	else 
		txt$(txt_item)&=chr$(9)&chr$(9)&'(no support data)'
	end if 
	! /r
next c_has_item
mat txt$(txt_item+=1) : txt$(txt_item)=''
mat txt$(txt_item+=1) : txt$(txt_item)='Business Rules! Version '&wbversion$
mat txt$(txt_item+=1) : txt$(txt_item)=chr$(9)&'Serial Number '&str$(serial)
mat txt$(txt_item+=1) : txt$(txt_item)=''
mat txt$(txt_item+=1) : txt$(txt_item)='For support contact ACS at:'
mat txt$(txt_item+=1) : txt$(txt_item)=chr$(9)&'1-800-643-6318'
mat txt$(txt_item+=1) : txt$(txt_item)=chr$(9)&'info@advancedcomputer.services'
mat txt$(txt_item+=1) : txt$(txt_item)=chr$(9)&'planetacs.net'
mat txt$(txt_item+=1) : txt$(txt_item)=''
mat txt$(txt_item+=1) : txt$(txt_item)=chr$(9)&'Advanced Computer Services LLC'
mat txt$(txt_item+=1) : txt$(txt_item)=chr$(9)&'4 Syme Ave'
mat txt$(txt_item+=1) : txt$(txt_item)=chr$(9)&'West Orange, NJ 07052'
fnmsgbox(mat txt$, response$, cap$)
goto XIT
XIT: fnxit
include: ertn
