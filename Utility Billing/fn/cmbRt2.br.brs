! Replace S:\acsUB\CmbRt2.br
! create a screen ace combo box for available Route Numbers
def library fncmbrt2(myline,mypos; disableAll)
	! disableAll (0=add option [All] to list, 1=dont show an option for disableAll)
	autoLibrary
	dim option$(999)
	fncreg_read('Route Low',bkno1$) : bkno1=val(bkno1$)
	fncreg_read('Route High',bkno2$) : bkno2=val(bkno2$)
	if disableAll=1 then 
		x=1
	else 
		option$(1)="[All]"
		x=2
	end if 
	for j=bkno1 to bkno2
		option$(x)=str$(j)
		x+=1
	next j
	mat option$(x-1)
	fnComboA("cmbRt2",myline,mypos,mat option$,"If the Route Number you want is not listed you can change your route number range in Company>Configure.",5)
fnend 
