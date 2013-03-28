 CREATE TABLE IF NOT EXISTS Problems (
           problemId serial primary key,
           name text unique not null,
           nbvars int,
           nbcons int,
           display text unique);



       CREATE TABLE IF NOT EXISTS Configs (
           configId serial primary key,
           config xml not null,
           md5 char(32) unique not null);

       
            CREATE TABLE IF NOT EXISTS Executions (
              executionId serial primary key,
              version integer not null,
              configId integer not null REFERENCES Configs on delete cascade,
              problemId integer not null REFERENCES Problems on delete cascade, 
              start timestamp not null,
              "end" timestamp null,
	      hostname text,
              solution text,
              unique(version, configId, problemId));

       CREATE TABLE IF NOT EXISTS ProblemTags (
           tag text not null,
           problemId integer not null REFERENCES Problems on delete cascade,
           primary key (tag, problemId));

       CREATE TABLE IF NOT EXISTS Statistics (
           name text not null,
           executionId integer not null REFERENCES Executions on delete cascade,
           value text not null, 
           primary key (name, executionId));

create or replace function stat(text, int) returns text as $$
  select value from statistics where (name, executionId) = ($1, $2);
$$ language sql;

