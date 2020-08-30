### R Intro - Final Exercise


# Packages
library(DBI)
library(odbc)
library(dplyr)


### ODBC Driver
driver <- "Driver={SQL Server Native Connection 11.0};Server=DESKTOP-VA3UHPO\\SQLEXPRESS;Database=Colleage;Trusted_Connection=True;"


### DB Connection
con <- dbConnect(odbc(), 
                 Driver = "SQL Server", 
                 Server = "DESKTOP-VA3UHPO\\SQLEXPRESS", 
                 Database = "Colleage", 
                 Trusted_Connection = "True")


### Get all COLLEGE tables
students = dbGetQuery(con, "SELECT * FROM dbo.students")
courses = dbGetQuery(con, "SELECT * FROM dbo.courses")
clasrooms = dbGetQuery(con, "SELECT * FROM dbo.classrooms")
departments = dbGetQuery(con, "SELECT * FROM dbo.departments")
teachers = dbGetQuery(con, "SELECT * FROM dbo.teachers")

#####  Questions #####

# Q1. Count the number of students on each department

df1 <- merge( clasrooms, courses,by = "CourseId" )
df1 <- merge( df1, departments, by = "DepartmentId" )

df1 %>% 
  group_by(DepartmentName) %>% 
  summarise(studentCount=length(unique(StudentId)))


# Q2. How many students have each course of the English department and the total number of students in the department?

df2 <- merge( clasrooms, courses,by = "CourseId" )

data_depId1 = df2 %>% 
  filter(DepartmentId == 1)

# head(data_depId1)
data_depId1 %>% 
  group_by(CourseId) %>% 
  summarise(studentCount=length(unique(StudentId)))

total = length(unique(data_depId1$StudentId))
total



# Q3. How many small (<22 students) and large (22+ students) classrooms are needed for the Science department?

df3 <- merge( clasrooms, courses,by = "CourseId" )
df3 <- df3 %>% filter(DepartmentId == 2)
gb_df3 <- df3 %>% group_by(CourseName) %>% summarise(stdCnt = length(unique(StudentId)),.groups = 'drop')
# gb_df3

gb_df3 <- gb_df3 %>% 
          mutate("classroom_size" = case_when(stdCnt >= 22 ~ 'Big_Clasrooms', TRUE ~ 'Small_Clasrooms'))

gb_df3_classize <- gb_df3 %>% group_by(classroom_size) %>% summarise(n(),.groups = 'drop')
gb_df3_classize

# Q4. A feminist student claims that there are more male than female in the College. Justify if the argument is correct

df4 <- students %>% group_by(Gender) %>% summarise(stdNum = length(unique(StudentId)), .groups = 'drop')
df4

# Q5. For which courses the percentage of male/female students is over 70%?

df5 <- merge(x=clasrooms,y=courses,by="CourseId",all.x=TRUE)
df5 <- merge(x=df5,y=departments,by="DepartmentId",all.x=TRUE)
df5 <- merge(x=df5,y=students,by="StudentId",all.x=TRUE)
# df5

courses_count_df5 <- df5 %>% group_by(CourseName) %>% summarise(stdCourseTot = length(StudentId), .groups = 'drop')
courses_count_df5
gb_df5 <- df5 %>% group_by(CourseName,Gender) %>% summarise(stdByGenInCoursTot=length(StudentId), .groups = 'drop')
gb_df5 <- merge(x=gb_df5,y=courses_count_df5,by="CourseName",all.x=TRUE)

gb_df5 <- mutate(gb_df5, gender_prec = stdByGenInCoursTot / stdCourseTot)

over_gender_prec <- gb_df5 %>% filter(gender_prec > 0.7)
over_gender_prec

# Q6. For each department, how many students passed with a grades over 80?

df6 <- df5
df6 <- df6 %>% mutate("studentover80" = if_else(degree >80, 1, 0))
df6 <-df6[order(df6$StudentId,df6$CourseId, -df6$degree),]
df6_distinct <- df6 %>% distinct(df6$StudentId, df6$DepartmentId, .keep_all = TRUE)

gb_df6 <- df6_distinct %>% group_by(DepartmentName) %>% summarise(stdUniq = length(unique(StudentId)),student80=sum(studentover80), .groups = 'drop')
gb_df6 <- mutate(gb_df6, student80_prec =  (student80*100)/ stdUniq)
gb_df6

# Q7. For each department, how many students passed with a grades under 60?

df7 <- df5
df7 <- df7 %>% mutate("studentbelow60" = if_else(degree <60, 1, 0))
df7 <-df7[order(df7$DepartmentId,df7$StudentId,df7$degree ),]
df7_distinct <- df7 %>% distinct(df7$DepartmentId,df7$StudentId, df7$degree, .keep_all = TRUE)
df7_distinct
gb_df7 <- df7_distinct %>% group_by(DepartmentName) %>% summarise(stdUniq = length(unique(StudentId)),student60=sum(studentbelow60), .groups = 'drop')
gb_df7 <- mutate(gb_df7, student60_prec =  (student60*100)/ stdUniq)
gb_df7

# Q8. Rate the teachers by their average student's grades (in descending order).

df8 <- merge(x=clasrooms,y=courses,by="CourseId",all.x=TRUE)
df8 <- merge(x=df8,y=teachers,by="TeacherId",all.x=TRUE)
df8 <- mutate(df8, Teacher =(paste(FirstName,LastName,sep=" ")))
df8$Teacher <- trimws(df8$Teacher, which = c("both"))


gb_df8 <- df8 %>% group_by(Teacher) %>% summarise(avg_degrees=mean(degree), .groups = 'drop')
gb_df8 <-gb_df8[order(-gb_df8$avg_degrees ),]
gb_df8 <- na.omit(gb_df8)
print(gb_df8,right=F)


# Q9. Create a dataframe showing the courses, departments they are associated with, the teacher in each course, and the number of students enrolled in the course (for each course, department and teacher show the names).

df9 <- merge( departments, courses,by = "DepartmentId" )
df9 <- merge(df9, teachers, by="TeacherId")
df9 <- merge(df9,clasrooms, by="CourseId")

gb_df9 <- df9 %>% group_by(CourseName,DepartmentName,FirstName,LastName) %>% summarise(num_students = length(unique(StudentId)))
gb_df9

# Q10. Create a dataframe showing the students, the number of courses they take, the average of the grades per class, and their overall average (for each student show the student name).

df10 <- merge(x=students,y=clasrooms,by="StudentId",all.x=TRUE)
df10 <- merge(x=df10,y=courses,by="CourseId",all.x=TRUE)
df10 <- df10 %>% mutate("English_degree" = ifelse(DepartmentId == 1, degree,NA))
df10 <- df10 %>% mutate("Science_degree" = ifelse(DepartmentId == 2, degree,NA))
df10 <- df10 %>% mutate("Arts_degree" = ifelse(DepartmentId == 3, degree,NA))
df10 <- df10 %>% mutate("Sports_degree" = ifelse(DepartmentId == 4, degree,NA))



gb_df10 <- df10 %>% 
            group_by(StudentId,FirstName,LastName) %>% 
            summarise(courses_num = length(unique(CourseId)),English = mean(na.omit(English_degree)) , Arts = mean(na.omit(Arts_degree)),
                      Science = mean(na.omit(Science_degree)), Sports = mean(na.omit(Sports_degree)), General = mean(degree),.groups = 'drop')

gb_df10


