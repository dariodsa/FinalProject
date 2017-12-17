#include <iostream>
#include <string.h>
#include <string>
#include <stdlib.h>
#include <vector>
#include <map>
using namespace std;
inline double function1(double x,double y)
{
	return x*x+y*y;
}
inline double function2(double x,double y)
{
	return x*x + (y-90.0)*(y-90.0);
}

struct Trie
{
	vector<int>V[9000];
	
	vector<int>query(double minValue,double maxValue)
	{
	
	}
	
	void update(int id,double oldValue, double newValue)
	{
		
	}
};

Trie T1;
Trie T2;

vector<int> combine(vector<int>first, vector<int>second)
{
	map<int,int>mapa;
	vector<int>ans;
	for(int i=0,len=first.size();i<len;++i)
		mapa[first[i]]=1;
	for(int i=0,len=second.size();i<len;++i)
	{
		if(mapa[second[i]]==1)
		{
			ans.push_back(second[i]);
		}
	}
}

struct Point
{
	int id;
	double x;
	double y;
	double dx;
	double dy;
	double f1;
	double f2;
	Point(double _x,double _y,double _dx,double _dy)
	{
		x=_x;
		y=_y;
		dx=_dx;
		dy=_dy;
	}
	void move()
	{
		double newFunctionValue = function1(x+dx,y+dy);
		T1.update(id,f1,newFunctionValue);
		f1 = newFunctionValue;
		newFunctionValue = function2(x+dx,y+dy);
		T2.update(id,f2,newFunctionValue);
		f2 = newFunctionValue;
		x=x+dx;
		y=y+dy;
		
	}
	void update()
	{}
};
struct Query
{
	double x;
	double y;
	double radius;
	Query(double _x,double _y, double _radius)
	{
		x=_x;
		y=_y;
		radius=_radius;
	}
	void ask()
	{
		double minValueFirst,minValueSecond;
		double maxValueFirst,maxValueSecond;
		
		vector<int> first = T1.query(minValueFirst,maxValueFirst);
		vector<int> second = T2.query(minValueSecond,maxValueSecond);
		vector<int> result = combine(first,second);
		printf("GOT IT!!\n");
	}
};
vector<Point>points;
vector<Query>queries;
void loadData(char *filePath)
{
	double x,y,dx,dy;
	FILE *pfile = fopen ( filePath, "r");
	while(fscanf(pfile,"%lf,%lf,%lf,%lf",&x,&y,&dx,&dy)!=EOF)
	{
		points.push_back(Point(x,y,dx,dy));
	}
}

void loadQuerys(char *filePath)
{
	double x,y,radius;
	FILE *pfile = fopen ( filePath, "r");
	while(fscanf(pfile,"%lf,%lf,%lf",&x,&y,&radius)!=EOF)
	{
		queries.push_back(Query(x,y,radius));
	}
}
int main(int argc, char** argv) {
	
	loadData(argv[1]);
	loadQuerys(argv[2]);
	while(true)
	{
		for(int i=0,len=points.size();i<len;++i)
			points[i].move();
		for(int i=0,len=queries.size();i<len;++i)
			queries[i].ask();
	}
	return 0;
}
