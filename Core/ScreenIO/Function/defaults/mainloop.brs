def fnMainLoop
  ! 1000-1500 are safe to use for whatever I want
  ! 2500+ is reserved for screenio
  if fkey=1 or fkey=1504 then 
    if env$('Program_Caption')='Select Company' then 
      help_cursys$='co'
    else 
      help_cursys$=lwrc$(env$('CurSys'))
    end if 
    ! pr 'help_cursys$='&help_cursys$ : pause
    execute 'system -M start http://planetacs.net/help/'&help_cursys$&'/'&srep$(env$('Program_Caption'),' ','%20')&'.html'
  else if fkey=1505 then 
    fnprogram_properties
  end if
fnend
