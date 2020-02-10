let rec msb n = 
  if n = 0
  then 0
  else 1 + msb(n lsr 1);;



