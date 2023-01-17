countbouts=function(thresh=30){
 selout=selectData()
 selected=selout$data
 name=selout$name
 bcounts(selected,name,thresh=thresh)
}
