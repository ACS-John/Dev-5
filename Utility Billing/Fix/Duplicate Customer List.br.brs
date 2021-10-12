	autoLibrary
	on error goto Ertn

	dim z$*10,e$(4)*30,dat$*20,idx$(5)*40
	dim item1$(6)*22
	dim a2(10),a(10),extra$(11)*30

	fnTop(program$)
	fndat(dat$,1)
	on fkey 5 goto DONE
	fnopenprn
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],Shr",i,i,r
	open #2: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,i,k
	fn_header
	do
		read #1,using FORM_CUSTOMER: z$,mat e$,mat a,final,bal,route,sequence,extra$(1) eof DONE
FORM_CUSTOMER: form pos 1,c 10,pos 11,4*c 30,pos 143,5*pd 2,pos 1806,3*n 2,pos 153,2*pd 2,pos 1821,n 1,pos 292,pd 4.2,pos 1741,n 2,n 7,pos 1864,c 30
		if fn_has_dupe(z$) then
			pr #255,using FORM_OUT: rec(1),z$,e$(2),e$(1),bal,mat a2 pageoflow PgOf
FORM_OUT: form n 4,x 1,c 10,x 5,c 30,x 7,c 30,n 11.2,x 1,10*nz 3
		end if  ! fn_has_dupe(z$)
	loop
DONE: !
	fncloseprn
Xit: !
	fnXit

PgOf: !
	pr #255: newpage
	fn_header
	continue

def fn_has_dupe(z$)
		hd_return=0
		z_one$=z_two$=''
		restore #2:
		read #2,using FORM_CUSTOMER,key=z$: z_one$
		read #2,using FORM_CUSTOMER: z_two$ eof HD_EOF
HD_EOF: !
		if z_one$=z_two$ then hd_return=1
		fn_has_dupe=hd_return
fnend  ! fn_has_dupe
def fn_header
		pr #255: "\qc {\b "&env$('cnam')&"}"
		pr #255: "\qc {\fs28 {\b Duplicate Customer List}}"
		pr #255: "\qc {\b "&trim$(item1$(1))&" Order}"
		pr #255,using 'form pos 21,cc 40,pos 71,c 5,pic(zzz)': dat$,"Page ",p2+=1
		pr #255: ""
		pr #255: "\ql {\ul     Customer No}     {\ul Name                             }    {\ul Meter Address                }   {\ul   Balance} {\ul}"
fnend

include: ertn

