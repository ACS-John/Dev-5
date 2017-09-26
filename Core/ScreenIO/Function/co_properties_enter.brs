def fnco_properties_enter
  
  library 'S:\Core\Library': fnread_program_print_property
  dim program_ini_file$*256
  if env$('Core_Program_Current')='Core\ScreenIO\ScreenIO\PROPERTIES.br' then
    setenv('Core_Program_Current','CO Properties Screen Test')
  end if
  
  fnread_program_print_property('Orientation' ,s$(sio_cmbOrientation) )
  fnread_program_print_property('Height'      ,s$(sio_txtHeight)      )
  fnread_program_print_property('Width'       ,s$(sio_txtWidth)       )
  fnread_program_print_property('Lines'       ,s$(sio_txtLpp)         )
  fnread_program_print_property('FontSize'    ,s$(sio_txtFontSize)    )
  fnread_program_print_property('TopMargin'   ,s$(sio_txtMarginTop)   )
  fnread_program_print_property('BottomMargin',s$(sio_txtMarginBottom))
  fnread_program_print_property('LeftMargin'  ,s$(sio_txtMarginLeft)  )
  fnread_program_print_property('RightMargin' ,s$(sio_txtMarginRight) )
  if env$('ACSDeveloper')<>'' then 
    s$(sio_lblIniFile)='Developer detected'
  end if 

fnend
 