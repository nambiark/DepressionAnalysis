select count(*) from depression.mental_heath limit 10

##Top 10 countries with depression incidence
select avg(val) as incidence, location from `depression`.`mental_health`  where measure ='Incidence' group by location 
order by incidence desc limit 10;

##Top 10 countries and income group with depression incidence
select avg(val) as incidence, location, w.`Income group` from `depression`.`mental_health` as m left join 
`depression`.`worldbankclassification` as w on m.location=w.Country 
 where m.measure ='Incidence' group by m.location, w.`Income group` order by incidence desc limit 10;
 
###Gender with highest level of depression per year
select avg(val) as incidence, year,sex from `depression`.`mental_health`  where measure ='Incidence' 
group by sex,year  order by incidence desc

##Age group with highest level of depression 
select avg(val) as incidence, age from `depression`.`mental_health`  where measure ='Incidence' 
group by age  order by incidence desc

##Age group and sex with highest level of depression
select avg(val) as incidence, age,sex from `depression`.`mental_health`  where measure ='Incidence' 
group by age,sex  order by incidence desc


