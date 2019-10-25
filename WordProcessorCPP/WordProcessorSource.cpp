#include <iostream>
#include <fstream>
#include <string>
#include <vector>
using namespace std;

#define MAX 256

void sort_str(string& in_str)
{
	//cout << "sort: " << in_str << endl;
	char temp;
	for (unsigned int i = 1; i < in_str.size(); i++)
	{
		for (unsigned int j = 0; j < in_str.size() - i; j++)
		{
			if (in_str[j + 1] < in_str[j])
			{
				temp = in_str[j + 1];
				in_str[j + 1] = in_str[j];
				in_str[j] = temp;
			}
		}
	}
	//cout << "sorted: " << in_str << endl;
}

void remove_duplicates_rec(string& in_str, char current, int i)
{
	//cout << "before dup: " << in_str << endl;
	if (in_str.size() < 1 || i == in_str.size())
		return;
	else if (current == in_str[i])
	{
		in_str.erase(i,1);
		remove_duplicates_rec(in_str, current, i);
	}
	else
		remove_duplicates_rec(in_str, in_str[i], i+1);
	//cout << "after dup: " << in_str << endl;
}

void remove_duplicates(string& in_str)
{
	//cout << "before dup: " << in_str << endl;
	char current = in_str[0];
	for (unsigned int i = 1; i < in_str.size(); i++)
	{
		if (current == in_str[i])
		{
			in_str.erase(i,1);
			i--;
		}
		else
			current = in_str[i];
	}
	//cout << "after dup: " << in_str << endl;
}

bool havesamec(const string& test_str1, const string& test_str2)
{
	string temp1 = test_str1;
	string temp2 = test_str2;
	sort_str(temp1);
	//remove_duplicates(temp1);
	remove_duplicates_rec(temp1,temp1[0],1);
	sort_str(temp2);
	//remove_duplicates(temp2);
	remove_duplicates_rec(temp2,temp2[0],1);
	if (temp1 == temp2)
		return true;
	else return false;
}

void allsamec(const string& in_str, const vector<string>& FileContents, vector<string>& output)
{
	for (unsigned int i = 0; i < FileContents.size(); i++)
	{
		if (havesamec(in_str, FileContents[i]))
			output.push_back(FileContents[i]);
	}
}

int main()
{
	ifstream infile;
	ofstream outfile;
	string infilename;
	string outfilename;
	string searchstr;
	vector<string> FileContents;
	vector<string> SearchResults;

	cout << "Please enter input file: ";
	cin >> infilename;

	infile.open(infilename.c_str());

	if (infile)
	{
		char buffer[MAX];
		string tempstr;
		while (infile)
		{
			infile.getline(buffer,MAX);
			tempstr = buffer;
			FileContents.push_back(tempstr);
		}
	}
	else
	{
		cout << "File not found" << endl;
		return 1;
	}
	infile.close();

	for (unsigned int i = 0; i < FileContents.size(); i++)
		cout << FileContents[i] << endl;

	cout << "enter a string: ";
	cin >> searchstr;

	allsamec(searchstr, FileContents, SearchResults);

	for (unsigned int i = 0; i < SearchResults.size(); i++)
		cout << SearchResults[i] << endl;

	searchstr = searchstr + ".txt";
	outfile.open(searchstr.c_str());
	for (unsigned int i = 0; i < SearchResults.size(); i++)
		outfile << SearchResults[i] << endl;
	outfile.close();

	return 0;
}