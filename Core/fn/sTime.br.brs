def fnstime_(_stime$)
	if len(_stime$)<>8 then _stime$=fntime_parse$(_stime$)
	_shh=_smm=_sss=0 
	_stime$=srep$(_stime$,":","") 
	_shh=val(_stime$(1:2)) conv L11778 
	_smm=val(_stime$(3:4)) conv L11778 
	_sss=val(_stime$(5:6)) conv L11778
L11778: fnstime_=(_shh*3600+_smm*60+_sss)*100
fnend 
def library fnstime(_stime$)
	if last_stime$=_stime$ then 
		fnstime=last_stime 
	else 
		last_stime$=_stime$ 
		fnstime=last_stime=fnstime_(_stime$)
	end if
fnend 
def fn_stime$(_stime)
	_stime=abs(_stime/100)
	_shh=int(_stime/3600) : _stime-=(_shh*3600) 
	_smm=int(_stime/60): _stime-=(_smm*60) 
	_sss=_stime
	if _shh<=99 then 
		fn_stime$=cnvrt$('PIC(##:##:##)',_shh*10000+_smm*100+_sss) 
	else 
		fn_stime$=str$(_shh)&":"&cnvrt$('PIC(##:##)',_smm*100+_sss)
	end if
fnend 
def library fnstime$(_stime)
	if last_stime=_stime then 
		fnstime$=last_stime$ 
	else 
		last_stime=_stime 
		fnstime$=last_stime$=fn_stime$(_stime)
	end if
fnend 
def fntime_parse$(_stime$)
! The Function parses out unwanted chars from the time like AM 
	! Formats time ##:##:##
	pm_time=0 
	_stime$=srep$(uprc$(_stime$),"AM","")
	if pos(uprc$(_stime$),"PM")>0 then 
		_stime$=srep$(uprc$(_stime$),"PM","") 
		pm_time=1
	end if
	_stime$=trim$(_stime$) 
	_phh=_pmm=_pss=0 
	pos_hh=pos_mm=0
	pos_hh=pos(_stime$,":") 
	pos_mm=pos(_stime$,":",pos_hh+1)
	_phour$=(_stime$(1:pos_hh-1)) 
	_phour=val(phour$) conv ignore 
	if pm_time and _phour<>12 then 
		_phour=12+_phour 
		_phour$=str$(_phour) 
	else if pm_time=0 and _phour=12 then 
		_phour=0 
		_phour$=str$(_phour)
	end if
	if len(_phour$)<2 then _phour$=lpad$(_phour$,2)
	_pmin$=(_stime$(pos_hh+1:pos_mm-1)) 
	if len(_pmin$)<2 then _pmin$=lpad$(_pmin$,2)
	_psec$=(_stime$(pos_mm+1:pos_mm+2)) 
	if len(_psec$)<2 then _psec$=lpad$(_psec$,2)
	fntime_parse$=_phour$&":"&_pmin$&":"&_psec$
fnend 