fn_setup
fnTop(program$)
if ~fnclient_has_on_support_item('CO', 60) then
	fnUpdateLicense
end if
if fnclient_has_on_support_item('CO', 60) then
	execute 'Sy -M -c start https://acs.servicecamp.com//'
else
	dim message$(0)*256
	mat message$(1)
	message$(1)='Only users currently on support may open service tickets.'
	fnmsgbox(mat message$)
end if
fnXit
def fn_setup
	setup=1
	autoLibrary
	on error goto Ertn
fnend
include: ertn
