#Смирнова Полина Алексеевна — для региона 82 рассчитайте урожайность пшеницы в период с 2009 по 2016 год взяв для рассчета средние суммы активных температур за эти годы, с 18 ближайших метеостанций
#Регион Крым
getwd()
setwd("C:/smirnova")
getwd()

# открываем нужные пакеты
library(dplyr)
library(tidyverse)
library(rnoaa)
library(lubridate)
#скачиваем станции
station_data = ghcnd_stations()
station_data
write.csv(station_data, file = "stations.csv")
station_data = read.csv("stations.csv")
#45.286090, 34.098512
#После получения всписка всех станций, получите список станций ближайших к столице вашего региона,создав таблицу с именем региона и координатами его столицы
krim = data.frame(id = "KRIM", latitude = 45.286090,  longitude = 34.098512)
krim_around = meteo_nearby_stations(lat_lon_df = krim, station_data = station_data,
                                    limit = 18, var = c("TAVG"),
                                    year_min = 2009, year_max = 2016)
#krim_around - это список элементов, которого является таблица, содержащая идентификаторы метеостанций отсортированных по их удаленности от Крыма.
#Очевидно, что первым элементом таблицы будет идентификатор метеостанции Крым. Его мы и получим
krim_id = krim_around [["KRIM"]][["id"]][1]
summary(krim_id)
#Для получения таблицы со всеми метеостанциями вокруг Крыма необходимо выбрать целиком первый объекта из списка
krim_table=krim_around[[1]]
summary(krim_table)
#В таблице krim_table оказалось 18 объектов ранжированных по расстоянию от Крыма. Сформируем список необходимых станций, выведем идентификатор отфильтрованных метеостанций
krim_stations=krim_table
str(krim_stations)
#Выведем идентификатор отфильтрованных метеостанций
krim_stations$id
#Чтобы получить все данные с одной метеостанции, используем команду meteo_tidy_ghcnd
all_krim_data=meteo_tidy_ghcnd(stationid = krim_id)

summary(all_krim_data)
#Создадим объект, куда скачиваем все данные всех метеостанций (количество), создаем цикл для наших метеостанций
all_krim_meteodata = data.frame()

stations_names=krim_stations$id
stations_names=stations_names[1:18] 


for (sname in stations_names)
{ one_meteo=meteo_tidy_ghcnd( stationid = sname,
                              date_min = "2009-01-01",
                              date_max = "2016-12-31")
station_vars=names(one_meteo)
if (!("tavg" %in% station_vars)){
  if(!("tmax"%in% station_vars)){
    next()
  }
  
  
  
  
  one_meteo=one_meteo %>% mutate(tavg=(tmax+tmin)/2)}
one_meteo=one_meteo %>% select(id,date,tavg)
one_meteo = one_meteo %>% mutate(tavg=tavg/10)
all_krim_meteodata=rbind(all_krim_meteodata, one_meteo)}
#Записываем полученные результаты
write.csv(all_krim_meteodata,"all_krim_meteodata.csv")
#Считываем данные
all_Amurskoe_meteodata=read.csv("all_krim_meteodata.csv")

str(all_krim_meteodata)
#Добавим год, месяц, день
all_krim_meteodata=all_krim_meteodata %>% mutate(year=year(date), 
                                                 month=month(date), 
                                                 day=day(date))
#Превратим NA в ноль, и где TAVG<5
all_krim_meteodata[is.na(all_krim_meteodata$tavg),"tavg"] = 0
all_krim_meteodata[all_krim_meteodata$tavg<5, "tavg"] = 0
summary(all_krim_meteodata)
#Сгруппируем метеостанции по id, месяцам и годам, просуммируем температуру по этим группам, затем сгруппируем данные по месецам и найдем среднее по месяцам по всем метеостанциям
group_meteodata =all_krim_meteodata %>% group_by(id,year,month)
sumT_group_meteodata = group_meteodata %>% summarise(tsum=sum(tavg))
groups_month=sumT_group_meteodata%>%group_by(month)
sumT_month=groups_month%>%summarise(St=mean(tsum))

### константа
afi = c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000)
# константа
bfi = c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000)
# отношение числа дней i-го месяца, входящих в период вегетации культуры, к общему числу дней в месяце
di = c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000)
# коэффициент для экпозиции склона - считаем что все поля идеально ровные
y=1.0
# коэффициент использования ФАР посевом
Kf=300
# калорийность урожая культуры 
Qj=1600
# коэффициент «Сумма частей основной и побочной продукции
Lj=2.2
# коэффициент «Стандартная влажность культуры»
Ej=25
#Рассчитываем Fi по месецам
sumT_month =sumT_month %>% mutate(Fi = afi+bfi*y*St)
#Рассчитаем Yi
sumT_month = sumT_month %>% mutate( Yi = ((Fi*di)*Kf)/(Qj*Lj*(100-Ej)))
##  Рассчитаем урожай
Yield = (sum(sumT_month$Yi)) 
Yield 
# 12,85049
