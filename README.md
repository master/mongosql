README
======

This is the SQL wrapper for [MongoDB](http://www.mongodb.org) for Erlang. It mimics interface and behavior of MySQL Native Erlang driver.

INSTALL
=======

Download and compile MongoSQL and eMongo driver

	$ git clone git://github.com/master/emongo.git emongo
	$ git clone git://github.com/master/mongosql.git mongosql
 	$ cd emongo
 	$ make
 	$ cd ../mongosql
 	$ make
 	$ cd ..

USAGE
=====
	$ erl -pa emongo/ebin mongosql/ebin

	> application:start(emongo).
	> {ok, Pool} = mongosql_conn:start([{"127.0.0.1", 27017}], "test1").
	> mongosql_conn:fetch(Pool, "insert into tbl1 col1, col2 values ('string', 1);", false).
	{updated,1}
	> mongosql_conn:fetch(Pool, "select count(*) from tbl1;", false). 
	{selected,["count(*)"],[{"1"}]}
	> mongosql_conn:fetch(Pool, "select * from tbl1;", false).
	{selected,["col1","col2"],[["string",1]]}
	> mongosql_conn:fetch(Pool, "delete from tbl1 where col2 = 1;", false).  
	{updated,1}
	> mongosql_conn:fetch(Pool, "select count(*) from tbl1;", false).        
	{selected,["count(*)"],[{"0"}]}
	> mongosql_conn:stop(Pool).


[![Bitdeli Badge](https://d2weczhvl823v0.cloudfront.net/master/mongosql/trend.png)](https://bitdeli.com/free "Bitdeli Badge")

