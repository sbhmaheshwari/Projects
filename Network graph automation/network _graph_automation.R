{
  library(osmar)
  library(sp)
  library(mapview)
  library(dplyr)
  library(leaflet)
}
setwd("F:/Old_Desktop/UC Davis/Research GSR/Networked graph/bin")
load("PreMap.RData")
load("Orange.Rdata")
#-------------------------------------------------------------------------------------------
{
  #bbox = corner_bbox(-118.1174,33.5076,-117.5056,33.9462) # orange county
  bbox = corner_bbox(-118.0042, 33.6363, -117.7226, 33.9194)
  #src = osmsource_osmosis(file = "california-latest.osm", osmosis = "osmosis")
  #data = get_osm(bbox, source = src)
  hw_ids = find(data, way(tags(k %grep% "highway" & v %grep% "motorway"
  )))
  hw_ids = find_down(data, way(hw_ids))
  hw = subset(data, ids = hw_ids)
  hw_lines = as_sp(hw, "lines")
  hw_nodes = as_sp(hw, "points")
}
save(hw, file = "highway_data.Rdata")
#-----------------------------------------------------------------------------------------
#Extract nodes that connect 2 links
{
  uniw = unique(hw$ways$attrs$id)
  node_req = c()
  for (i in 1:length(uniw)){
    nodesw = hw$ways$refs$ref[which(hw$ways$refs$id == uniw[i])]
    nodesw = nodesw[which(nodesw %in% hw$nodes$attrs$id)]
    if(length(nodesw)>1){
      nodesw_hw = c(nodesw[1], nodesw[length(nodesw)])
      node_req = c(node_req, nodesw_hw)
    }
  }
  hp_req = find_up(hw, node(unique(node_req)))
  hp = subset(hw, ids = hp_req)
  hp_req_sp = as_sp(hp, "points")
}
save(hp_req_sp, file = "hp_req_sp.Rdata")
save(hw_lines, file = "hw_lines.Rdata")
#-----------------------------------------------------------------------------------------
#remove links that are at the ends
mapview(hw_lines) + mapview(hp_req_sp, color = "black", cex = 1)
#leaflet::addCircleMarkers(hp_req_sp, lat = hp_req_sp$lat, lng = hp_req_sp$lon)#, lat = 33.803, lng = -117.953)
#-----------------------------------------------------------------------------------------
# Create Link Matrix
load("LinkMat.Rdata")
# {
# LinkMat = data.frame(matrix(0,nrow = length(unique(node_req)),
#                             ncol = length(unique(hp_req$way_ids))))
# names(LinkMat) = hp$ways$attrs$id
# rownames(LinkMat) = unique(node_req)
# refMat = hp$ways$refs
# for (i in 1:ncol(LinkMat)){
#   idc = names(LinkMat)[i]
#   nodeVec = which(refMat$id == idc)
#   nodeVec = refMat$ref[nodeVec]
#   matchref = nodeVec[nodeVec %in% unique(node_req)]
#   if (length(matchref) >= 2){
#     LinkMat[as.character(matchref)[1],i] = -1
#     LinkMat[as.character(matchref)[length(matchref)],i] = +1
#   }
#   else{
#     LinkMat[,i] = NA
#   }
# }
# rmVec = which(apply(LinkMat, 2, sum) == 0)
# LinkMat = LinkMat[,rmVec]
# }
#-----------------------------------------------------------------------------------------
{
  MetaData = read.table(
    "F:/Old_Desktop/UC Davis/Research GSR/Station_Hr_Data/MetaData/d012_text_meta.txt",
    header = TRUE,
    sep = "\t",
    fill = TRUE)
  
  m = leaflet() %>% addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
    addPolylines(data = hw_lines, highlightOptions = highlightOptions(color = "red"),
                 label = ~as.character(id)) %>%
    addCircleMarkers(data = hp_req_sp, radius = 0.3, col = "black",
                     label = ~as.character(id)) %>%
    addCircleMarkers(data = MetaData, col = "red", radius = 0.5,
                     label = ~as.character(ID))
  
  NodeMat = hp$ways$ref %>% filter(id %in% names(LinkMat))
  Fwy = lapply(unique(NodeMat$id), function(x){
    a = hp$ways$tags %>% filter(id == x) 
    if (a$v[which(a$k == "highway")] == "motorway"){
    a = a %>% filter(k == "ref"| k =="name") 
    }
  })
  Fwy = plyr::ldply(Fwy, as.data.frame)
  FwynameList = as.vector(unique(Fwy$v[which(Fwy$k=="name")]))
  FwynameList = cbind(FwynameList, c("CA 22","CA 55","CA 133 Toll",
                                     "I 5", "I 405", "CA 133", "CA 241 Toll",
                                     "CA 91","CA 91","CA 73","CA 57","CA 91 Toll",
                                     "CA 90","CA 73 Toll"))
  FwynameList_rep = FwynameList[match(Fwy$v, FwynameList[,1]),2]
  Fwy$v[!is.na(FwynameList_rep)] = FwynameList_rep[!is.na(FwynameList_rep)]
  Fwy = Fwy %>% group_by(id) %>% filter(row_number()==1)%>%ungroup()
  Fwy = Fwy[,-2]
  Fwy = Fwy %>% mutate(Fwyno = as.numeric(grep("[0-9]",
                                               unlist(strsplit(as.character(v), split = " ")),
                                               value = TRUE))) %>%
    arrange(Fwyno) %>% select(-v) %>% mutate(dir = rep(0,nrow(Fwy)))
  Fwy = split(Fwy, Fwy$Fwyno)
  Fwy = lapply(Fwy, function(x){x = x[,-2]})
  n = as.numeric(names(Fwy))
  SensorData = MetaData %>% filter(Fwy %in% n) %>% arrange(Fwy)
  SensorData = split(SensorData, SensorData$Fwy)
  Fwy = Fwy[which(names(Fwy) %in% names(SensorData))]
}
#-----------------------------------------------------------------------------
# Assigning direction to each freeway link
for (i in 1:length(Fwy)){
  for (j in 1:nrow(Fwy[[i]])){
    l = Fwy[[i]]$id[j]
    Node_order1 = rownames(LinkMat)[which(LinkMat[,
                                                  which(names(LinkMat) == l)] == -1)]
    Node_order2 = rownames(LinkMat)[which(LinkMat[,
                                                  which(names(LinkMat) == l)] == 1)]
    Node_coord = hw$nodes$attrs[which(hw$nodes$attrs$id %in% c(Node_order1,
                                                               Node_order2)),
                                c("id","lat","lon")]
    dr = MetaData$Dir[MetaData$Fwy == names(Fwy)[i]][1]
    if (dr %in% c("N","S")){
      order1 = Node_coord[which(Node_coord$id == Node_order1)[1],"lat"]
      order2 = Node_coord[which(Node_coord$id == Node_order2)[1],"lat"]
      w = order1>order2
      if(w){
        Fwy[[i]][j,2] = "S"
      }
      else{
        Fwy[[i]][j,2] = "N"
      }
    }
    if (dr %in% c("E","W")){
      order1 = Node_coord[which(Node_coord$id == Node_order1)[1],"lon"]
      order2 = Node_coord[which(Node_coord$id == Node_order2)[1],"lon"]
      w = order1>order2
      if (w){
        Fwy[[i]][j,2] = "W"
      }
      else{
        Fwy[[i]][j,2] = "E"
      }
    }
  }
}
#---------------------------------------------------------------------------------
# add immediate motorway-links to Fwy
Fwy_mtr = Fwy
for (i in 1:length(Fwy)){
  for (j in 1:nrow(Fwy[[i]])){
    l_mtr = Fwy[[i]]$id[j]
    n1_mtr = hp$ways$refs$ref[which(hp$ways$ref$id %in% l_mtr)]
    n_mtr = n1_mtr[n1_mtr %in% hp$nodes$attrs$id]
    nin_mtr = hp$nodes$attrs[which(hp$nodes$attrs$id %in% n_mtr),
                             c("id","lat","lon")]
    n_mtr = nin_mtr[match(n_mtr,nin_mtr$id),]
    nlast_mtr = n_mtr[nrow(n_mtr),]
    n1_mtr = n_mtr[1,]
    nlast_mtr_len = length(which(LinkMat[as.character(nlast_mtr$id),] %in% 
                                   c(1,-1)))
    n1_mtr_len = length(which(LinkMat[as.character(n1_mtr$id),] %in% 
                                c(1,-1)))
    
    idx_mtr_last = names(LinkMat)[which(LinkMat[as.character(nlast_mtr$id), ] %in%
                                          c(1, -1))]
    idx_mtr = idx_mtr_last[-which(idx_mtr_last %in% Fwy[[i]]$id)]
    if (length(idx_mtr) > 0) {
      row_mtr = data.frame(id = as.numeric(idx_mtr), dir = rep(as.character(Fwy[[i]]$dir[j], length(idx_mtr))))
      Fwy_mtr[[i]] = rbind(Fwy_mtr[[i]], row_mtr)
    }
    idx_mtr_1 = names(LinkMat)[which(LinkMat[as.character(n1_mtr$id), ] %in%
                                       c(1, -1))]
    idx_mtr = idx_mtr_1[-which(idx_mtr_1 %in% Fwy[[i]]$id)]
    if (length(idx_mtr) > 0) {
      row_mtr = data.frame(id = as.numeric(idx_mtr), dir = rep(as.character(Fwy[[i]]$dir[j], length(idx_mtr))))
      Fwy_mtr[[i]] = rbind(Fwy_mtr[[i]], row_mtr)
    }
    
    if (length(n_mtr$id)>2){
      n_mtr = n_mtr[-c(1,nrow(n_mtr)),]
      id_mtr = which(rownames(LinkMat) %in% n_mtr$id)
      idx_mtr = sapply(id_mtr, function(x){
        names(LinkMat)[which(LinkMat[x,] %in% c(1,-1))]
      })
      row_mtr = data.frame(id = as.numeric(idx_mtr), dir = rep(as.character(
        Fwy[[i]]$dir[j], length(idx_mtr))))
      Fwy_mtr[[i]] = rbind(Fwy_mtr[[i]], row_mtr)
    }
  }
}
Fwy_mtr = lapply(Fwy_mtr, function(x){
  x %>% group_by(id) %>% filter(row_number()==1) %>% ungroup()
})
#---------------------------------------------------------------------------------
# Break the links having more than 2 nodes in LinkMat
{
Mat = rep(0, nrow(LinkMat))
linkName = c()
for (i in 1:ncol(LinkMat)){
  l = as.numeric(names(LinkMat))[i]
  n1 = hp$ways$refs$ref[which(hp$ways$ref$id %in% l)]
  n = n1[n1 %in% hp$nodes$attrs$id]
  nin = hp$nodes$attrs[which(hp$nodes$attrs$id %in% n), c("id","lat","lon")]
  n = nin[match(n,nin$id),]
  
  if(length(n$id)>2){
    linkName = c(linkName,l)
    nsize = nrow(n)
    dimname = paste(l, "-", 1:(nsize-1), sep = "")
    NewMat_linkMat = as.data.frame(matrix(0, nrow = nrow(LinkMat),
                                          ncol = nsize-1,
                                          dimnames = list(c(rownames(LinkMat)),
                                                          c(dimname))))
    for (k in 1:ncol(NewMat_linkMat)){
      NewMat_linkMat[as.character(n$id[k]),k] = -1
      NewMat_linkMat[as.character(n$id[k+1]),k] = +1
    }
    Mat = cbind(Mat, NewMat_linkMat)
  }
}
LinkMat = cbind(LinkMat, Mat)
LinkMat = LinkMat[,-which(names(LinkMat) %in% linkName)]
LinkMat = LinkMat[,-which(names(LinkMat) == "Mat")]
}
save(LinkMat, file = "LinkMat_Final.RData")
#-----------------------------------------------------------------------------
## To create sensor link relation
#load("linkId.Rdata")
# {
# linkId = list()
# for (i in 1:length(SensorData)){
#   NodesReq = NodeMat %>% filter(id %in% Fwy_mtr[[i]]$id)
#   LatLonNode = hw$nodes$attrs[(which(hw$nodes$attrs$id %in% NodesReq$ref)),
#                               c("id","lat","lon")]
#   names(LatLonNode)[1] = "ref"
#   x = lapply(NodesReq$ref, function(x){LatLonNode[which(LatLonNode$ref == x)[1],
#                                                   c("lat","lon")]})
#   x = plyr::ldply(x, as.data.frame)
#   NodesReq = data.frame(NodesReq,x)
#   linkId[[i]] = as.data.frame(matrix(0,nrow = nrow(SensorData[[i]]),
#                                      ncol = 3))
#   SensorData2 = SensorData[[i]] %>% group_by(Latitude,Longitude) %>% filter(row_number()==1) %>% ungroup()
#   print(i)
#   j=1; k=1
#   while(j <= nrow(SensorData2)){
#     if(j %% 10 == 0){print(j)}
#     LatLonSens = SensorData2[j,c("Latitude", "Longitude")]
#     idx = lapply(unique(NodesReq$id), function(x){
#       n = NodesReq %>% filter(id == x)
#       distNodes = sqrt(diff(n$lat)^2+diff(n$lon)^2)
#       DistPoint1 = sqrt(((LatLonSens$Latitude-n$lat)[-nrow(n)])^2 +
#                           (LatLonSens$Longitude-n$lon)[-nrow(n)]^2)
#       DistPoint2 = sqrt(((LatLonSens$Latitude-n$lat)[-1])^2 +
#                           (LatLonSens$Longitude-n$lon)[-1]^2)
#       idx = DistPoint1+DistPoint2 - distNodes
#       data.frame(id = n$id[-nrow(n)],idx)
#     })
#     idx = plyr::ldply(idx, as.data.frame)
#     link = which.min(abs(idx$idx))
#     current_sens = SensorData[[i]][which(SensorData[[i]]$Latitude == LatLonSens$Latitude &
#                            SensorData[[i]]$Longitude == LatLonSens$Longitude),"ID"]
#     if (idx$idx[link] < 10^-4){
#     linkId[[i]][k:(k+length(current_sens)-1),] = as.data.frame(matrix(c(rep(idx[link,1],length(current_sens)),
#                           current_sens, rep(idx[link,2], length(current_sens))), byrow = F, ncol = 3))
#     }
#     else{
#      linkId[[i]][k:(k+length(current_sens)-1),] = as.data.frame(matrix(c(rep(NA,length(current_sens)),
#                                                                          current_sens, rep(idx[link,2],
#                                                                          length(current_sens))), byrow = F, ncol = 3))
#     }
#   j = j+1
#   k = k+length(current_sens)
#     }
# }
# }
#---------------------------------------------------------------------------------
# Map sensors on divided links appropriately
load("linkId.Rdata")
z2 = 0
for (i in 1:length(Fwy_mtr)) {
  for (j in 1:nrow(Fwy_mtr[[i]])) {
    l = Fwy_mtr[[i]]$id[j]
    n1 = hp$ways$refs$ref[which(hp$ways$ref$id %in% l)]
    n = n1[n1 %in% hp$nodes$attrs$id]
    nin = hp$nodes$attrs[which(hp$nodes$attrs$id %in% n), c("id", "lat", "lon")]
    n = nin[match(n, nin$id),]
    
    if (length(n$id) > 2) {
      sens = linkId[[i]]$V2[which(linkId[[i]]$V1 %in% l)]
      sensInfo = SensorData[[i]][which(SensorData[[i]]$ID %in% sens),
                                 c("ID", "Latitude", "Longitude")]
      if (length(sens) > 0){
        z2 = z2 + 1
        dr = Fwy_mtr[[i]]$dir[j]
        if (dr %in% c("S", "N")) {
          id = sapply(sensInfo$Latitude, function(x) {
            if (dr == "S") {
              which(x >= sort(n$lat, decreasing = TRUE))[1]
            }
            else {
              which(x <= sort(n$lat))[1]
            }
          })
        }
        if (dr %in% c("W", "E")) {
          id = sapply(sensInfo$Longitude, function(x) {
            if (dr == "W") {
              which(x >= sort(n$lon, decreasing = TRUE))[1]
            }
            else{
              which(x <= sort(n$lon))[1]
            }
          })
        }
        NewMat_linkId = matrix(0, nrow = length(sens), ncol = 3)
        NewMat_linkId[, 2] = sens
        linkname = names(LinkMat)[which(names(LinkMat) %grep% l)]
        NewMat_linkId[, 1] = sapply(id, function(x){
          if (is.na(x)){
            linkname[length(linkname)]
          }else if (x==1){
            linkname[x]
          }else{
            linkname[x-1]
          }
        })
        linkId[[i]] = rbind(linkId[[i]], NewMat_linkId)
        linkId[[i]] = linkId[[i]][-which(linkId[[i]]$V1 == l),]
        }
      }
    }
  }
save(linkId, file = "linkId_premap.Rdata")
#m %>% addCircleMarkers(lat = sensInfo$Latitude, lng = sensInfo$Longitude, col = "red", label = as.character(sensInfo$ID))
#----------------------------------------------------------------------------------
# Re-creating Fwy_mtr with break links
matnew = Fwy_mtr
for (i in 1:length(Fwy_mtr)){
  for (j in 1:nrow(Fwy_mtr[[i]])){
    idx_new = names(LinkMat)[which(names(LinkMat) %grep% Fwy_mtr[[i]]$id[j])]
    if (length(idx_new) > 1){
      matnew[[i]] = rbind(matnew[[i]], matrix(c(idx_new, 
                                                rep(Fwy_mtr[[i]]$dir[j], 
                                                    length(idx_new))), byrow = FALSE,
                                              ncol = 2,
                                              dimnames = list(c(), c("id", "dir"))))
      matnew[[i]] = matnew[[i]][-which(matnew[[i]]$id==Fwy_mtr[[i]]$id[j]),]
    }
    
  }
}
matnew_Fwy = Fwy
for (i in 1:length(Fwy)){
  for (j in 1:nrow(Fwy[[i]])){
    idx_new = names(LinkMat)[which(names(LinkMat) %grep% Fwy[[i]]$id[j])]
    if (length(idx_new) > 1){
      matnew_Fwy[[i]] = rbind(matnew_Fwy[[i]], matrix(c(idx_new, 
                                                rep(Fwy[[i]]$dir[j], 
                                                    length(idx_new))), byrow = FALSE,
                                              ncol = 2,
                                              dimnames = list(c(), c("id", "dir"))))
      matnew_Fwy[[i]] = matnew_Fwy[[i]][-which(matnew_Fwy[[i]]$id==Fwy[[i]]$id[j]),]
    }
    
  }
}
#----------------------------------------------------------------------------------
# Mapping different type of sensors
{
Meta_type = split(MetaData, MetaData$Type)
p = leaflet() %>% addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
  addPolylines(data = hw_lines, highlightOptions = highlightOptions(color = "red"),
               label = paste(as.character(hw_lines$id),as.character(hw_lines$user),
                         as.character(hw_lines$uid))) %>%
  addCircleMarkers(data = hp_req_sp, radius = 0.3, col = "black",
                   label = ~as.character(id))
cols = c("white", "cyan", "magenta", "pink", "orange", "green","red")
pal = colorFactor(cols, unique(MetaData$Type))
purrr::walk(names(Meta_type), function(x) {
  dat = Meta_type[[x]]
  p <<- p %>%
    addCircleMarkers(
      data = dat,
      radius = 3,
      col = pal(x),
      popup = c(paste(
        as.character(dat$ID),
        as.character(dat$Type),
        as.character(dat$User_ID_1),
        as.character(dat$Name)
      )),
      group = x,
      fillOpacity = 0.5,
      opacity = 0.8
    )})
p = p %>% addLayersControl(overlayGroups = names(Meta_type),
                         options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(names(Meta_type)[-which(names(Meta_type) == "ML")]) %>%
  leaflet::addLegend(position = "bottomright", 
                     pal = pal,
                     values = names(Meta_type),
                     title = "Sensors")
p
save(p, file = "County_Map.Rdata")
#save.image("PreMap.RData")
plotpoint = function(x){
  p %>% addCircleMarkers(lat = hw$nodes$attrs$lat[which(hw$nodes$attrs$id %in% x)], 
                         lng = hw$nodes$attrs$lon[which(hw$nodes$attrs$id %in% x)], 
                         label = as.character(hw$nodes$attrs$id[which(hw$nodes$attrs$id %in% x)]))
}
save(MetaData, file = "MetaData.Rdata")
}
#----------------------------------------------------------------------------------
# Extract the nodes connecting freeway to ramps
findLink = function(link){
  node_link = rownames(LinkMat)[LinkMat[,which(names(LinkMat) == link)] %in% 
                                  c(1,-1)]
  link1 = names(LinkMat)[which(LinkMat[node_link[1],] %in% c(1,-1))]
  link1 = link1[-which(link1 %in% link)]
  link2 = names(LinkMat)[which(LinkMat[node_link[2],] %in% c(1,-1))]
  link2 = link2[-which(link2 %in% link)]
  return(list(link1, link2))
}

rmFF_int = function(x, currFwy){
  d_int = c()
  currFwy = currFwy
  matrix_int = x
  for (i in 1:nrow(x)){
    conn_link_int = unlist(findLink(x$id[i]))
    fwy_int = unique(conn_link_int[which(conn_link_int %in% All_Fwy$id)])
    if(length(fwy_int)==4){
      d_int = c(d_int,i)
    }
  }
  if(length(d_int)>0){
    matrix_int = matrix_int[-d_int,]
  }
  return(list(matrix_int, d_int))
}

rmFF = function(x, currFwy){
  d = c()
  matrix = x
  currFwy = currFwy
  for (i in 1:nrow(x)){
    if(i%%5 == 0)print(i)
    prevlink = c()
    p_FF = 2
    link = x$id[i]
    conn_link = unlist(findLink(link))
    idx_FF = conn_link[-unlist(sapply(All_Fwy$id, function(x){
      which(conn_link %grep% x)}))]
    if (length(idx_FF) == 0){
      dir = All_Fwy_mtr$.id[which(All_Fwy_mtr$id %in% unique(conn_link))]
      if(length(dir[which(dir!=currFwy)])>0 & length(dir)==length(conn_link)){p_FF=3}
    }
    else{
      while(length(idx_FF)!=0 & p_FF == 2){
        prevlink = c(prevlink, link)
        link = idx_FF
        conn_link = c(unlist((sapply(link, function(x){unlist(findLink(x))}))))
        idx_FF = conn_link[-which(conn_link %in% c(prevlink, link))]
        if(length(idx_FF) == 0){p_FF = 0}
        check_FF = unlist(sapply(All_Fwy_mtr$id, function(x){which(idx_FF == x)}))
        if(length(check_FF)!=0){
          dir = All_Fwy_mtr$.id[which(All_Fwy_mtr$id %in% idx_FF[check_FF])]
          if (sum(dir != as.character(currFwy))!=0){
            p_FF = 3
          }
          else{
            idx_FF = unique(idx_FF[-which(dir==as.character(currFwy))])
          }
        }
      } 
    }
    if (p_FF==3){
      d = c(d,i)
    }
  }
  if (length(d)>0){
    matrix = matrix[-d,]
  }
  return(list(matrix,d))
}

b = find(hw, way(tags(k %grep% "highway" & v == "motorway_link")))
node_ramp = hw$ways$refs$ref[which(hw$ways$refs$id %in% b)]
All_Fwy_mtr = plyr::ldply(matnew, as.data.frame)
All_Fwy = plyr::ldply(matnew_Fwy, as.data.frame)
a = list()
node_Fwy = list()
node_ramp_Fwy = list()
OR = list()
FR = list()
OR_2 = list()
FR_2 = list()
Meta_OR = list()
Meta_FR = list()
for (i in 1:length(Fwy)){
  print(i)
  a[[i]] = Fwy[[i]]$id
  node_Fwy[[i]] = hw$ways$refs$ref[which(hw$ways$refs$id %in% a[[i]])]
  node_ramp_Fwy[[i]] = node_ramp[which(node_ramp %in% node_Fwy[[i]])]
  OR[[i]] = as.data.frame(matrix(0, nrow = 1, ncol = 2,
                                 dimnames = list(c(), c("ref","id"))))
  FR[[i]] = as.data.frame(matrix(0, nrow = 1, ncol = 2,
                                 dimnames = list(c(), c("ref","id"))))
  for (j in 1:length(node_ramp_Fwy[[i]])){
    row = which(rownames(LinkMat) == node_ramp_Fwy[[i]][j])
    if (length(row)>0){
      plus = names(LinkMat)[which(LinkMat[row,] == +1)]
      minus = names(LinkMat)[which(LinkMat[row,] == -1)]
      plus_ramp = unlist(sapply(b, function(x){plus[which(plus %grep% x)]}))
      minus_ramp = unlist(sapply(b, function(x){minus[which(minus %grep% x)]}))
      OR[[i]] = rbind(OR[[i]],matrix(c(rep(node_ramp_Fwy[[i]][j],length(unique(plus_ramp)))
                             ,unique(plus_ramp)), byrow = FALSE, ncol = 2,
                           dimnames = list(c(), c("ref","id"))))
      FR[[i]] = rbind(FR[[i]],matrix(c(rep(node_ramp_Fwy[[i]][j],length(unique(minus_ramp)))
                             ,unique(minus_ramp)), byrow = FALSE, ncol = 2,
                           dimnames = list(c(), c("ref","id"))))
    }
  }
  OR_2[[i]] = OR[[i]][-1,]; rownames(OR_2[[i]]) = NULL
  FR_2[[i]] = FR[[i]][-1,]; rownames(FR_2[[i]]) = NULL
  save(OR, file = "OR.RData")
  save(FR, file = "FR.RData")
  
  OR_2[[i]] = rmFF_int(OR_2[[i]],names(Fwy)[i])[[1]]
  FR_2[[i]] = rmFF_int(FR_2[[i]],names(Fwy[i]))[[1]]
  
  OR_2[[i]] = rmFF(OR_2[[i]], names(Fwy)[i])[[1]]
  FR_2[[i]] = rmFF(FR_2[[i]], names(Fwy)[i])[[1]]
  
  Coord_Node_ramp_Fwy = hw$nodes$attrs[(which(hw$nodes$attrs$id %in% node_ramp_Fwy[[i]])),
                                       c("id", "lat", "lon")] 
  Coord_Node_ramp_Fwy = Coord_Node_ramp_Fwy %>%  group_by(id) %>% filter(row_number() == 1) %>% ungroup()
  names(Coord_Node_ramp_Fwy)[1] = "ref"
  
  link = NodeMat[which(NodeMat$ref %in% Coord_Node_ramp_Fwy$ref),]
  NodeMat_ramp_Fwy = link %>% group_by(ref) %>% filter(row_number() == 1) %>% ungroup()
  
  dir_ramp_Fwy = sapply(NodeMat_ramp_Fwy$id, function(x){Fwy_mtr[[i]]$dir[which(Fwy_mtr[[i]]$id == 
                                                                   x)]})
  NodeMat_ramp_Fwy = NodeMat_ramp_Fwy %>% mutate(dir = dir_ramp_Fwy)
  
  OR_2[[i]] = OR_2[[i]] %>% mutate(dir = NodeMat_ramp_Fwy$dir[match(OR_2[[i]]$ref, NodeMat_ramp_Fwy$ref)],
                         lat = Coord_Node_ramp_Fwy$lat[match(OR_2[[i]]$ref, Coord_Node_ramp_Fwy$ref)],
                         lon = Coord_Node_ramp_Fwy$lon[match(OR_2[[i]]$ref, Coord_Node_ramp_Fwy$ref)]) 
  OR_2[[i]] = OR_2[[i]] %>% group_by(id) %>% filter(row_number()==1) %>% ungroup()
  OR_2[[i]] = split(OR_2[[i]], OR_2[[i]]$dir) 
  
  FR_2[[i]] = FR_2[[i]] %>% mutate(dir = NodeMat_ramp_Fwy$dir[match(FR_2[[i]]$ref, NodeMat_ramp_Fwy$ref)],
                         lat = Coord_Node_ramp_Fwy$lat[match(FR_2[[i]]$ref, Coord_Node_ramp_Fwy$ref)],
                         lon = Coord_Node_ramp_Fwy$lon[match(FR_2[[i]]$ref, Coord_Node_ramp_Fwy$ref)])
  FR_2[[i]] = FR_2[[i]] %>% group_by(id) %>% filter(row_number()==1) %>% ungroup()
  FR_2[[i]] = split(FR_2[[i]], FR_2[[i]]$dir)
  
  Meta_ramp_Fwy = SensorData[[i]]
  Meta_OR[[i]] = list()
  Meta_FR[[i]] = list()
  
  if (sum(unique(dir_ramp_Fwy) %in% c("N","S")) == 2){
    if(length(OR_2[[i]]$N)>0)OR_2[[i]]$N = OR_2[[i]]$N %>% arrange(desc(lat))
    if(length(OR_2[[i]]$S)>0)OR_2[[i]]$S = OR_2[[i]]$S %>% arrange(lat)
    if(length(FR_2[[i]]$N)>0)FR_2[[i]]$N = FR_2[[i]]$N %>% arrange(lat)
    if(length(FR_2[[i]]$S)>0)FR_2[[i]]$S = FR_2[[i]]$S %>% arrange(desc(lat))
    
    Meta_ramp_Fwy = Meta_ramp_Fwy %>% filter(Latitude < max(Coord_Node_ramp_Fwy$lat) &
                                               Latitude > min(Coord_Node_ramp_Fwy$lat))
    Meta_ramp_Fwy = split(Meta_ramp_Fwy, Meta_ramp_Fwy$Type, drop = TRUE)
    Meta_ramp_Fwy = lapply(Meta_ramp_Fwy, function(x){split(x, x$Dir, drop = TRUE)})
    
    if(length(Meta_ramp_Fwy$OR$N)>0)Meta_OR[[i]]$N = Meta_ramp_Fwy$OR$N %>% arrange(desc(Latitude)) %>% 
      select(ID, Fwy, Dir, Latitude, Longitude)
    if(length(Meta_ramp_Fwy$OR$S)>0)Meta_OR[[i]]$S = Meta_ramp_Fwy$OR$S %>% arrange(Latitude) %>% 
      select(ID, Fwy, Dir, Latitude, Longitude)
    if(length(Meta_ramp_Fwy$FR$N)>0)Meta_FR[[i]]$N = Meta_ramp_Fwy$FR$N %>% arrange(Latitude)%>% 
      select(ID, Fwy, Dir, Latitude, Longitude)
    if(length(Meta_ramp_Fwy$FR$S)>0)Meta_FR[[i]]$S = Meta_ramp_Fwy$FR$S %>% arrange(desc(Latitude))%>% 
      select(ID, Fwy, Dir, Latitude, Longitude)
  }
  else{
    if(length(OR_2[[i]]$E)>0)OR_2[[i]]$E = OR_2[[i]]$E %>% arrange(desc(lon))
    if(length(OR_2[[i]]$W)>0)OR_2[[i]]$W = OR_2[[i]]$W %>% arrange(lon)
    if(length(FR_2[[i]]$E)>0)FR_2[[i]]$E = FR_2[[i]]$E %>% arrange(lon)
    if(length(FR_2[[i]]$W)>0)FR_2[[i]]$W = FR_2[[i]]$W %>% arrange(desc(lon))
    
    Meta_ramp_Fwy = Meta_ramp_Fwy %>% filter(Longitude < max(Coord_Node_ramp_Fwy$lon) &
                                               Longitude > min(Coord_Node_ramp_Fwy$lon))
    Meta_ramp_Fwy = split(Meta_ramp_Fwy, Meta_ramp_Fwy$Type, drop = TRUE)
    Meta_ramp_Fwy = lapply(Meta_ramp_Fwy, function(x){split(x, x$Dir, drop = TRUE)})
    
    if(length(Meta_ramp_Fwy$OR$E)>0)Meta_OR[[i]]$E = Meta_ramp_Fwy$OR$E %>% arrange(desc(Longitude)) %>% 
      select(ID, Fwy, Dir, Latitude, Longitude)
    if(length(Meta_ramp_Fwy$OR$W)>0)Meta_OR[[i]]$W = Meta_ramp_Fwy$OR$W %>% arrange(Longitude) %>% 
      select(ID, Fwy, Dir, Latitude, Longitude)
    if(length(Meta_ramp_Fwy$FR$E)>0)Meta_FR[[i]]$E = Meta_ramp_Fwy$FR$E %>% arrange(Longitude)%>% 
      select(ID, Fwy, Dir, Latitude, Longitude)
    if(length(Meta_ramp_Fwy$FR$W)>0)Meta_FR[[i]]$W = Meta_ramp_Fwy$FR$W %>% arrange(desc(Longitude))%>% 
      select(ID, Fwy, Dir, Latitude, Longitude)
  }
}
names(OR_2) = names(Fwy)
names(FR_2) = names(Fwy)
names(Meta_OR) = names(Fwy)
names(Meta_FR) = names(Fwy)
names(node_Fwy) = names(Fwy)
save(All_Fwy, file = "link_fwy.Rdata")
save(NodeMat, file = "node_link_relation.Rdata")
#----------------------------------------------------------------------------------
OR_copy = OR_2
FR_copy = FR_2
OR_idx = list()
FR_idx = list()
ORFR_to_link = function(){
for (k in c(1:7, 9:length(Fwy))){
  OR_idx[[k]] = list()
  FR_idx[[k]] = list()
  if (sum(names(OR_copy[[k]]) %in% c("N", "S")) == 2){
    for (i in 1:length(Meta_OR[[k]])){
      if (i == 1){
        OR_idx[[k]]$N = data.frame(sensorId = 0, id = 0)
        j = 1
        while(j < nrow(Meta_OR[[k]]$N)+1){
          x3 = TRUE
          x1 = which(OR_copy[[k]]$N$lat>Meta_OR[[k]]$N[j,]$Latitude)
          x2 = which(OR_copy[[k]]$N$lat<Meta_OR[[k]]$N[j,]$Latitude)[1]
          if (j < nrow(Meta_OR[[k]]$N) & !is.na(x2)){
            dis = abs(Meta_OR[[k]]$N[j+1,]$Latitude - Meta_OR[[k]]$N[j,]$Latitude)
            if(Meta_OR[[k]]$N[j+1,]$Latitude>OR_copy[[k]]$N[x2,]$lat & (dis<0.01)){
              x3 = FALSE
            }      
          }
          if(length(x1) == 0){
            OR_idx[[k]]$N[j,] = c(Meta_OR[[k]]$N[j,]$ID, NA)
            j = j+1
          }
          else{
            if (length(x1)==1 & x3 == TRUE){
              if(abs(Meta_OR[[k]]$N$Latitude[j] - OR_copy[[k]]$N$lat[x1]) < 0.01){
                OR_idx[[k]]$N[j,] = c(Meta_OR[[k]]$N[j,]$ID, OR_copy[[k]]$N[x1,]$id)
              }else{
                OR_idx[[k]]$N[j,] = c(Meta_OR[[k]]$N[j,]$ID, NA)
              }
              OR_copy[[k]]$N = OR_copy[[k]]$N[-x1,]
              j = j+1
            }
            else if (length(x1) != 1 & x3 == TRUE){
              OR_idx[[k]]$N[j,] = c(Meta_OR[[k]]$N[j,]$ID, NA)
              OR_copy[[k]]$N = OR_copy[[k]]$N[-x1,]
              j = j+1
            }
            else{
              x4 = which(Meta_OR[[k]]$N[(j+1):nrow(Meta_OR[[k]]$N),]$Latitude>OR_copy[[k]]$N[x2,]$lat)
              OR_idx[[k]]$N[j:(length(x4)+j),] = data.frame(Meta_OR[[k]]$N[j:(length(x4)+j),]$ID, rep(NA, length(x4)+1))
              OR_copy[[k]]$N = OR_copy[[k]]$N[-x1,]
              j = j+length(x4)
            }
          }
        }
        OR_copy[[k]]$N = OR_2[[k]]$N[-which(OR_2[[k]]$N$id %in% OR_idx[[k]]$N$id),]
      }
      else{
        OR_idx[[k]]$S = data.frame(sensorId = 0, id = 0)
        j = 1
        while(j < nrow(Meta_OR[[k]]$S)+1){
          x3 = TRUE
          x1 = which(OR_copy[[k]]$S$lat<Meta_OR[[k]]$S[j,]$Latitude)
          x2 = which(OR_copy[[k]]$S$lat>Meta_OR[[k]]$S[j,]$Latitude)[1]
          if (j < nrow(Meta_OR[[k]]$S)& !is.na(x2)){
            dis = abs(Meta_OR[[k]]$S[j+1,]$Latitude - Meta_OR[[k]]$S[j,]$Latitude)
            if((Meta_OR[[k]]$S[j+1,]$Latitude<OR_copy[[k]]$S[x2,]$lat) & (dis < 0.01)){
              x3 = FALSE
            }
          }
          if(length(x1) == 0){
            OR_idx[[k]]$S[j,] = c(Meta_OR[[k]]$S[j,]$ID, NA)
            j = j+1
          }
          else{
            if (length(x1)==1 & x3 == TRUE){
              if(abs(Meta_OR[[k]]$S$Latitude[j] - OR_copy[[k]]$S$lat[x1]) < 0.01){
                OR_idx[[k]]$S[j,] = c(Meta_OR[[k]]$S[j,]$ID, OR_copy[[k]]$S[x1,]$id)
              }else{
                OR_idx[[k]]$S[j,] = c(Meta_OR[[k]]$S[j,]$ID, NA)
              }
              OR_copy[[k]]$S = OR_copy[[k]]$S[-x1,]
              j = j+1
            }
            else if (length(x1) != 1 & x3 == TRUE){
              OR_idx[[k]]$S[j,] = c(Meta_OR[[k]]$S[j,]$ID, NA)
              OR_copy[[k]]$S = OR_copy[[k]]$S[-x1,]
              j = j+1
            }
            else{
              x4 = which(Meta_OR[[k]]$S[(j+1):nrow(Meta_OR[[k]]$S),]$Latitude<OR_copy[[k]]$S[x2,]$lat)
              OR_idx[[k]]$S[j:(length(x4)+j),] = data.frame(Meta_OR[[k]]$S[j:(length(x4)+j),]$ID, rep(NA, length(x4)+1))
              OR_copy[[k]]$S = OR_copy[[k]]$S[-x1,]
              j = j+length(x4)
            }
          }
        }
        OR_copy[[k]]$S = OR_2[[k]]$S[-which(OR_2[[k]]$S$id %in% OR_idx[[k]]$S$id),]
      }
    }
    
    for (i in 1:length(Meta_FR[[k]])){
      if (i == 1){
        FR_idx[[k]]$N = data.frame(sensorId = 0, id = 0)
        j = 1
        while(j < nrow(Meta_FR[[k]]$N)+1){
          x3 = TRUE
          x1 = which(FR_copy[[k]]$N$lat<Meta_FR[[k]]$N[j,]$Latitude)
          x2 = which(FR_copy[[k]]$N$lat>Meta_FR[[k]]$N[j,]$Latitude)[1]
          if (j < nrow(Meta_FR[[k]]$N)& !is.na(x2)){
            dis = abs(Meta_FR[[k]]$N[j+1,]$Latitude - Meta_FR[[k]]$N[j,]$Latitude)
            if ((Meta_FR[[k]]$N[j+1,]$Latitude<FR_copy[[k]]$N[x2,]$lat) & (dis<0.01)){
              x3 = FALSE
            }
          }
          if(length(x1) == 0){
            FR_idx[[k]]$N[j,] = c(Meta_FR[[k]]$N[j,]$ID, NA)
            j = j+1
          }
          else{
            if (length(x1)==1 & x3 == TRUE){
              if(abs(Meta_FR[[k]]$N$Latitude[j] - FR_copy[[k]]$N$lat[x1]) < 0.01){
                FR_idx[[k]]$N[j,] = c(Meta_FR[[k]]$N[j,]$ID, FR_copy[[k]]$N[x1,]$id)
              }else{
                FR_idx[[k]]$N[j,] = c(Meta_FR[[k]]$N[j,]$ID, NA)
              }
              FR_copy[[k]]$N = FR_copy[[k]]$N[-x1,]
              j = j+1
            }
            else if (length(x1) != 1 & x3 == TRUE){
              FR_idx[[k]]$N[j,] = c(Meta_FR[[k]]$N[j,]$ID, NA)
              FR_copy[[k]]$N = FR_copy[[k]]$N[-x1,]
              j = j+1
            }
            else{
              x4 = which(Meta_FR[[k]]$N[(j+1):nrow(Meta_FR[[k]]$N),]$Latitude<FR_copy[[k]]$N[x2,]$lat)
              FR_idx[[k]]$N[j:(length(x4)+j),] = data.frame(Meta_FR[[k]]$N[j:(length(x4)+j),]$ID, rep(NA, length(x4)+1))
              FR_copy[[k]]$N = FR_copy[[k]]$N[-x1,]
              j = j+length(x4)
            }
          }
        }
        FR_copy[[k]]$N = FR_2[[k]]$N[-which(FR_2[[k]]$N$id %in% FR_idx[[k]]$N$id),]
      }
      else{
        FR_idx[[k]]$S = data.frame(sensorId = 0, id = 0)
        j = 1
        while(j < nrow(Meta_FR[[k]]$S)+1){
          x3 = TRUE
          x1 = which(FR_copy[[k]]$S$lat>Meta_FR[[k]]$S[j,]$Latitude)
          x2 = which(FR_copy[[k]]$S$lat<Meta_FR[[k]]$S[j,]$Latitude)[1]
          if (j < nrow(Meta_FR[[k]]$S) & !is.na(x2)){
            dis = abs(Meta_FR[[k]]$S[j+1,]$Latitude - Meta_FR[[k]]$S[j,]$Latitude)
            if ((Meta_FR[[k]]$S[j+1,]$Latitude>FR_copy[[k]]$S[x2,]$lat) & (dis<0.01)){
              x3 = FALSE
            }
          }
          if(length(x1) == 0){
            FR_idx[[k]]$S[j,] = c(Meta_FR[[k]]$S[j,]$ID, NA)
            j = j+1
          }
          else{
            if (length(x1)==1 & x3 == TRUE){
              if(abs(Meta_FR[[k]]$S$Latitude[j] - FR_copy[[k]]$S$lat[x1]) < 0.01){
                FR_idx[[k]]$S[j,] = c(Meta_FR[[k]]$S[j,]$ID, FR_copy[[k]]$S[x1,]$id)
              }else{
                FR_idx[[k]]$S[j,] = c(Meta_FR[[k]]$S[j,]$ID, NA)
              }
              FR_copy[[k]]$S = FR_copy[[k]]$S[-x1,]
              j = j+1
            }
            else if (length(x1) != 1 & x3 == TRUE){
              FR_idx[[k]]$S[j,] = c(Meta_FR[[k]]$S[j,]$ID, NA)
              FR_copy[[k]]$S = FR_copy[[k]]$S[-x1,]
              j = j+1
            }
            else{
              x4 = which(Meta_FR[[k]]$S[(j+1):nrow(Meta_FR[[k]]$S),]$Latitude>FR_copy[[k]]$S[x2,]$lat)
              FR_idx[[k]]$S[j:(length(x4)+j),] = data.frame(Meta_FR[[k]]$S[j:(length(x4)+j),]$ID, rep(NA, length(x4)+1))
              FR_copy[[k]]$S = FR_copy[[k]]$S[-x1,]
              j = j+length(x4)
            }
          }
        }
        FR_copy[[k]]$S = FR_2[[k]]$S[-which(FR_2[[k]]$S$id %in% FR_idx[[k]]$S$id),]
      }
    }
  }
  else{
    for (i in 1:length(Meta_OR[[k]])){
      if (i == 2){
        OR_idx[[k]]$E = data.frame(sensorId = 0, id = 0)
        j = 1
        while(j < nrow(Meta_OR[[k]]$E)+1){
          x3 = TRUE
          x1 = which(OR_copy[[k]]$E$lon>Meta_OR[[k]]$E[j,]$Longitude)
          x2 = which(OR_copy[[k]]$E$lon<Meta_OR[[k]]$E[j,]$Longitude)[1]
          if (j < nrow(Meta_OR[[k]]$E) & !is.na(x2)){
            dis = abs(Meta_OR[[k]]$E[j+1,]$Longitude - Meta_OR[[k]]$E[j,]$Longitude)
            if(Meta_OR[[k]]$E[j+1,]$Longitude>OR_copy[[k]]$E[x2,]$lon & (dis<0.01)){
              x3 = FALSE
            }      
          }
          if(length(x1) == 0){
            OR_idx[[k]]$E[j,] = c(Meta_OR[[k]]$E[j,]$ID, NA)
            j = j+1
          }
          else{
            if (length(x1)==1 & x3 == TRUE){
              if(abs(Meta_OR[[k]]$E$Longitude[j] - OR_copy[[k]]$E$lon[x1]) < 0.01){
                OR_idx[[k]]$E[j,] = c(Meta_OR[[k]]$E[j,]$ID, OR_copy[[k]]$E[x1,]$id)
              }else{
                OR_idx[[k]]$E[j,] = c(Meta_OR[[k]]$E[j,]$ID, NA)
              }
              OR_copy[[k]]$E = OR_copy[[k]]$E[-x1,]
              j = j+1
            }
            else if (length(x1) != 1 & x3 == TRUE){
              OR_idx[[k]]$E[j,] = c(Meta_OR[[k]]$E[j,]$ID, NA)
              OR_copy[[k]]$E = OR_copy[[k]]$E[-x1,]
              j = j+1
            }
            else{
              x4 = which(Meta_OR[[k]]$E[(j+1):nrow(Meta_OR[[k]]$E),]$Longitude>OR_copy[[k]]$E[x2,]$lon)
              OR_idx[[k]]$E[j:(length(x4)+j),] = data.frame(Meta_OR[[k]]$E[j:(length(x4)+j),]$ID, rep(NA, length(x4)+1))
              OR_copy[[k]]$E = OR_copy[[k]]$E[-x1,]
              j = j+length(x4)
            }
          }
        }
        OR_copy[[k]]$E = OR_2[[k]]$E[-which(OR_2[[k]]$E$id %in% OR_idx[[k]]$E$id),]
      }
      else{
        OR_idx[[k]]$W = data.frame(sensorId = 0, id = 0)
        j = 1
        while(j < nrow(Meta_OR[[k]]$W)+1){
          x3 = TRUE
          x1 = which(OR_copy[[k]]$W$lon<Meta_OR[[k]]$W[j,]$Longitude)
          x2 = which(OR_copy[[k]]$W$lon>Meta_OR[[k]]$W[j,]$Longitude)[1]
          if (j < nrow(Meta_OR[[k]]$W)& !is.na(x2)){
            dis = abs(Meta_OR[[k]]$W[j+1,]$Longitude - Meta_OR[[k]]$W[j,]$Longitude)
            if((Meta_OR[[k]]$W[j+1,]$Longitude<OR_copy[[k]]$W[x2,]$lon) & (dis < 0.01)){
              x3 = FALSE
            }
          }
          if(length(x1) == 0){
            OR_idx[[k]]$W[j,] = c(Meta_OR[[k]]$W[j,]$ID, NA)
            j = j+1
          }
          else{
            if (length(x1)==1 & x3 == TRUE){
              if(abs(Meta_OR[[k]]$W$Longitude[j] - OR_copy[[k]]$W$lon[x1]) < 0.01){
                OR_idx[[k]]$W[j,] = c(Meta_OR[[k]]$W[j,]$ID, OR_copy[[k]]$W[x1,]$id)
              }else{
                OR_idx[[k]]$W[j,] = c(Meta_OR[[k]]$W[j,]$ID, NA)
              }
              OR_copy[[k]]$W = OR_copy[[k]]$W[-x1,]
              j = j+1
            }
            else if (length(x1) != 1 & x3 == TRUE){
              OR_idx[[k]]$W[j,] = c(Meta_OR[[k]]$W[j,]$ID, NA)
              OR_copy[[k]]$W = OR_copy[[k]]$W[-x1,]
              j = j+1
            }
            else{
              x4 = which(Meta_OR[[k]]$W[(j+1):nrow(Meta_OR[[k]]$W),]$Longitude<OR_copy[[k]]$W[x2,]$lon)
              OR_idx[[k]]$W[j:(length(x4)+j),] = data.frame(Meta_OR[[k]]$W[j:(length(x4)+j),]$ID, rep(NA, length(x4)+1))
              OR_copy[[k]]$W = OR_copy[[k]]$W[-x1,]
              j = j+length(x4)
            }
          }
        }
        OR_copy[[k]]$W = OR_2[[k]]$W[-which(OR_2[[k]]$W$id %in% OR_idx[[k]]$W$id),]
      }
    }
    
    for (i in 1:length(Meta_FR[[k]])){
      if (i == 1){
        FR_idx[[k]]$E = data.frame(sensorId = 0, id = 0)
        j = 1
        while(j < nrow(Meta_FR[[k]]$E)+1){
          x3 = TRUE
          x1 = which(FR_copy[[k]]$E$lon<Meta_FR[[k]]$E[j,]$Longitude)
          x2 = which(FR_copy[[k]]$E$lon>Meta_FR[[k]]$E[j,]$Longitude)[1]
          if (j < nrow(Meta_FR[[k]]$E)& !is.na(x2)){
            dis = abs(Meta_FR[[k]]$E[j+1,]$Longitude - Meta_FR[[k]]$E[j,]$Longitude)
            if ((Meta_FR[[k]]$E[j+1,]$Longitude<FR_copy[[k]]$E[x2,]$lon) & (dis<0.01)){
              x3 = FALSE
            }
          }
          if(length(x1) == 0){
            FR_idx[[k]]$E[j,] = c(Meta_FR[[k]]$E[j,]$ID, NA)
            j = j+1
          }
          else{
            if (length(x1)==1 & x3 == TRUE){
              if(abs(Meta_FR[[k]]$E$Longitude[j] - FR_copy[[k]]$E$lon[x1]) < 0.01){
                FR_idx[[k]]$E[j,] = c(Meta_FR[[k]]$E[j,]$ID, FR_copy[[k]]$E[x1,]$id)
              }else{
                FR_idx[[k]]$E[j,] = c(Meta_FR[[k]]$E[j,]$ID, NA)
              }
              FR_copy[[k]]$E = FR_copy[[k]]$E[-x1,]
              j = j+1
            }
            else if (length(x1) != 1 & x3 == TRUE){
              FR_idx[[k]]$E[j,] = c(Meta_FR[[k]]$E[j,]$ID, NA)
              FR_copy[[k]]$E = FR_copy[[k]]$E[-x1,]
              j = j+1
            }
            else{
              x4 = which(Meta_FR[[k]]$E[(j+1):nrow(Meta_FR[[k]]$E),]$Longitude<FR_copy[[k]]$E[x2,]$lon)
              FR_idx[[k]]$E[j:(length(x4)+j),] = data.frame(Meta_FR[[k]]$E[j:(length(x4)+j),]$ID, rep(NA, length(x4)+1))
              FR_copy[[k]]$E = FR_copy[[k]]$E[-x1,]
              j = j+length(x4)
            }
          }
        }
        FR_copy[[k]]$E = FR_2[[k]]$E[-which(FR_2[[k]]$E$id %in% FR_idx[[k]]$E$id),]
      }
      else{
        FR_idx[[k]]$W = data.frame(sensorId = 0, id = 0)
        j = 1
        while(j < nrow(Meta_FR[[k]]$W)+1){
          x3 = TRUE
          x1 = which(FR_copy[[k]]$W$lon>Meta_FR[[k]]$W[j,]$Longitude)
          x2 = which(FR_copy[[k]]$W$lon<Meta_FR[[k]]$W[j,]$Longitude)[1]
          if (j < nrow(Meta_FR[[k]]$W) & !is.na(x2)){
            dis = abs(Meta_FR[[k]]$W[j+1,]$Longitude - Meta_FR[[k]]$W[j,]$Longitude)
            if ((Meta_FR[[k]]$W[j+1,]$Longitude>FR_copy[[k]]$W[x2,]$lon) & (dis<0.01)){
              x3 = FALSE
            }
          }
          if(length(x1) == 0){
            FR_idx[[k]]$W[j,] = c(Meta_FR[[k]]$W[j,]$ID, NA)
            j = j+1
          }
          else{
            if (length(x1)==1 & x3 == TRUE){
              if(abs(Meta_FR[[k]]$W$Longitude[j] - FR_copy[[k]]$W$lon[x1]) < 0.01){
                FR_idx[[k]]$W[j,] = c(Meta_FR[[k]]$W[j,]$ID, FR_copy[[k]]$W[x1,]$id)
              }else{
                FR_idx[[k]]$W[j,] = c(Meta_FR[[k]]$W[j,]$ID, NA)
              }
              FR_copy[[k]]$W = FR_copy[[k]]$W[-x1,]
              j = j+1
            }
            else if (length(x1) != 1 & x3 == TRUE){
              FR_idx[[k]]$W[j,] = c(Meta_FR[[k]]$W[j,]$ID, NA)
              FR_copy[[k]]$W = FR_copy[[k]]$W[-x1,]
              j = j+1
            }
            else{
              x4 = which(Meta_FR[[k]]$W[(j+1):nrow(Meta_FR[[k]]$W),]$Longitude>FR_copy[[k]]$W[x2,]$lon)
              FR_idx[[k]]$W[j:(length(x4)+j),] = data.frame(Meta_FR[[k]]$W[j:(length(x4)+j),]$ID, rep(NA, length(x4)+1))
              FR_copy[[k]]$W = FR_copy[[k]]$W[-x1,]
              j = j+length(x4)
            }
          }
        }
        FR_copy[[k]]$W = FR_2[[k]]$W[-which(FR_2[[k]]$W$id %in% FR_idx[[k]]$W$id),]
      }
    }
  }
}
names(FR_idx) = names(Fwy)
names(OR_idx) = names(Fwy)
return(list(FR_idx = FR_idx, OR_idx = OR_idx))
}
FR_idx = ORFR_to_link()[[1]]
OR_idx = ORFR_to_link()[[2]]
save.image("PreMap.RData")
#170 + 115 = 295
#249 + 224 = 473
for(i in c(1:7,9:10)){
  View(SensorData[[i]])
  for (j in names(OR_2[[i]])){
    q = plotpoint(OR_2[[i]][[which(names(OR_2[[i]])==j)]]$ref)
    print(q)
    View(OR_idx[[i]][[which(names(OR_idx[[i]])==j)]])
    View(OR_2[[i]][[which(names(OR_2[[i]])==j)]])
    for (k in 1:nrow(OR_idx[[i]][[which(names(OR_idx[[i]])==j)]])){
      if(is.na(OR_idx[[i]][[which(names(OR_idx[[i]])==j)]]$id[k])){
        print(k)
        OR_idx[[i]][[which(names(OR_idx[[i]])==j)]]$id[k] <- 
          readline(prompt = paste("SensorId = ", OR_idx[[i]][[which(names(OR_idx[[i]])==j)]][k,1], "Enter: "))
      }
    }
    plot.new()
  }
}
save(OR_idx, file = "OR_idx.Rdata")

for(i in c(1:7, 9:10)){
  View(SensorData[[i]])
  for (j in names(FR_2[[i]])){
    q = plotpoint(FR_2[[i]][[which(names(FR_2[[i]])==j)]]$ref)
    print(q)
    View(FR_idx[[i]][[which(names(FR_idx[[i]])==j)]])
    View(FR_2[[i]][[which(names(FR_2[[i]])==j)]])
    for (k in 1:nrow(FR_idx[[i]][[which(names(FR_idx[[i]])==j)]])){
      if(is.na(FR_idx[[i]][[which(names(FR_idx[[i]])==j)]]$id[k])){
        print(k)
        FR_idx[[i]][[which(names(FR_idx[[i]])==j)]]$id[k] <- 
          readline(prompt = paste("SensorId = ", FR_idx[[i]][[which(names(FR_idx[[i]])==j)]][k,1], "Enter: "))
      }
    }
    plot.new()
  }
}

save(FR_idx, file = "FR_idx.Rdata")
#---------------------------------------------------------------------------------------
# Mapping the Freeway-Freeway sensors
Meta_FF = list()
FF_idx = list()
Meta_FF = lapply(SensorData, function(x){x %>% filter(Type == "FF")})
for (i in 1:length(Meta_FF)){
  if(sum(unique(Meta_FF[[i]]$Dir) %in% c("N", "S"))==2){
    max_lat = max(Meta_OR[[i]]$N$Latitude)
    min_lat = min(Meta_OR[[i]]$N$Latitude)
    Meta_FF[[i]] = Meta_FF[[i]] %>% filter(Latitude < max_lat &
                                             Latitude > min_lat)
  }else{
    max_lng = max(Meta_OR[[i]]$E$Longitude)
    min_lng = min(Meta_OR[[i]]$E$Longitude)
    Meta_FF[[i]] = Meta_FF[[i]] %>% filter(Longitude < max_lng & Longitude > min_lng)
  }
  if(nrow(Meta_FF[[i]]) !=0){
    FF_idx[[i]] = data.frame(sensorId = Meta_FF[[i]]$ID, id = NA)
  }
}
names(FF_idx) = c("5", "22", "55", "57", "73", "91", "133")

for(i in 1){
  View(SensorData[[i]])
  View(FF_idx[[i]])
  for(j in 16){
    print(j)
    q = p %>% addCircleMarkers(lat = Meta_FF[[i]][j,]$Latitude, lng = Meta_FF[[i]][j,]$Longitude)
    print(q)
    FF_idx[[i]][j,2] = readline(prompt = paste("SensorId = ", FF_idx[[i]][j,1], 
                                              "Enter: ")) 
  }
    }
    plot.new()

save(FF_idx, file = "FF_idx.Rdata")
load("FR_idx.Rdata")
load("OR_idx.Rdata")

FF_idx_tot = plyr::ldply(FF_idx, as.data.frame)
FR_idx_tot = plyr::ldply(lapply(FR_idx, function(x)plyr::ldply(x, as.data.frame)), as.data.frame)
OR_idx_tot = plyr::ldply(lapply(OR_idx, function(x)plyr::ldply(x, as.data.frame)), as.data.frame)
# FF -> 91/128
# FR -> 209/224
# OR -> 247/249
p %>% addCircleMarkers(lat = Meta_FR[[1]]$S[2,]$Latitude, lng = Meta_FR[[1]]$S[2,]$Longitude)
