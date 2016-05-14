SELECT Sex,Education,avg(Earnings)
FROM census
group by Sex,Education