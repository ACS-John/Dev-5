library 'S:\Core\Library': fngethandle

!  dim internal_data$(0)*60,internal_data(0)
!  dim internal_fieldsc$(0)*20,internal_fieldsn$(0)*20
!  dim internal_fc$(1,3)*80,internal_fn$(1,3)*80
!  dim Internal_Form_C$*512,Internal_Form_N$*512
!  ! dim internal_formall$*512,internal_desc_c$(0)*80,internal_desc_n$(0)*80,internal_seq$(0)*80,internal_valid$(0)*80
!  library 'PROG2\INTERMNT.wb': fnopen_internal
!  relation_employer=10
!  fnopen_internal(relation_employer,h,file_name$, mat internal_data$, mat internal_data, mat internal_fieldsc$, mat internal_fieldsn$, internal_form_c$, internal_form_n$,mat internal_fc$,mat internal_fn$)
open #h:=fngethandle: "NAME=INTERNAL//6,KFNAME=INTERNAL.IDX//6,SHR",internal,input,keyed
pr 'file opened: ';file$(h)
fileno$='14901075'
dno=1
cnvrt$('n 3',relation_employer)
restore #h,key=rpad$(fileno$&cnvrt$('n 3',dno)&'10',kln(h)):
do
	dim iform$*256
	iform$='form pos 1,C 8,n 3,n 2,c 10,BH 3,c 30'
	dim acct_no$*30
	read #h,using iform$: fileno2$,dno2,rel2,type$,no,acct_no$
	pr fileno2$;' ';dno2;' ';rel2type$;' ';no;' ';acct_no$
loop while fileno$=fileno2$


