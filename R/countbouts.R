countbouts=function (thresh = 30) 
{
  selout = selectData()
  sframe= selout$data
  name = selout$name

  bcounts(sframe, name, thresh = thresh)
}