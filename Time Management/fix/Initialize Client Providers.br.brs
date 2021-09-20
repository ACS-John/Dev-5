fn_setup
dim c$(0)*256
dim cN(0)
hClient=fn_openFio('TM Client 420',mat c$,mat cN)
do
	read #hClient,using form$(hClient): mat c$,mat cN eof EoClient
	c$(client_provider)=fn_clientProvider$(c$(client_id))
	rewrite #hClient,using form$(hClient): mat c$,mat cN
loop
EoClient: !
goto Xit

def fn_clientProvider$*128(client$*64; ___,return$*128) ! stolen from Client - ideally upgrade the version in client after this provider field is initialized.
	client$=trim$(client$)
	if client$='ajj' then  ! American Jiu Jitsu of Maplewood
		! return$='John Bowman'
		return$='jb'
	else if client$='4132' or client$='3670' or client$='ped' then  ! Stern and Stern, Recoveries Unlimited and Peter Engler Designs
		! return$='Commercial Software Solutions LLC'
		return$='css'
	else
		! return$='Advanced Computer Services LLC'
		return$='acs'
	end if
	fnClientProvider$=return$
fnend

Xit: fnXit
include: fn_setup
include: fn_open

