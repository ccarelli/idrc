

dt[ ,population_total:=as.numeric(population_totale)]
new = dt[ ,.(population_totale=sum(population_totale, na.rm=T), 
      population_couverte=sum(population_couverte, na.rm=T),
      total_de_cas_incident=sum(total_de_cas_incident, na.rm=T), 
      total_de_cas=sum(total_de_cas, na.rm=T), 
      tb_teste_vih=sum(tb_teste_vih, na.rm=T), tb_vih_positif=sum(tb_vih_positif, na.rm=T),
      tb_vih_positif_tarv=sum(tb_vih_positif_tarv, na.rm=T),
      pvvih_sous_inh = sum(pvvih_sous_inh, na.rm=T)), by=.(dps, date)]

new[ ,tx:=tb_teste_vih/total_de_cas*100, by=.(dps,date)]
new[ ,tx:=round(tx, 1)]

ggplot(new, aes(x=date, y=tx, color=dps))+
  geom_point()+
  geom_line()+theme_bw()

list_of_plots = NULL
i = 1

for (d in new$dps) {
  name = as.character(d)
 list_of_plots[[i]] = ggplot(new[dps==d], aes(x=date, y=tx, color=dps))+
    geom_point()+
    geom_line()+theme_bw()+
   labs(title=paste0("Percent of TB cases tested for HIV: ", name),
     x='Date', y='Percent (%)')
 
 i = i+1
 }


pdf()

for (i in seq(length(list_of_plots))) {
  print(list_of_plots[[i]])
}

dev.off()