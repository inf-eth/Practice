/*
* test.cpp
*
*  Created on: 9 Mar 2017
*      Author: yz6316
*/
#include<iostream>
#include<vector>
#include<string>
#include<sstream>
#include<fstream>
#include<cstdlib>
using namespace std;

void allsamec(const vector<string>& a, const string& s, vector<string>& opp);
bool havesamec(const string& A, const string& S);
int main() {

	vector<string> text;

	ifstream infile;
	ofstream outfile;

	string infilename;
	cout << "enter input file name" << endl;
	cin >> infilename;

	infile.open(infilename.c_str());

	if (!infile.is_open()) {
		cout << "could not open input file" << endl;
		exit(EXIT_FAILURE);
	}

	string word;
	while (infile >> word) {
		text.push_back(word);
		cout << word << endl;
	}

	string x;
	cout << "enter a string" << endl;
	cin >> x;

	vector<string> op;

	allsamec(text, x, op);

	outfile.open(x.c_str());

	cout << "strings with all and only the same characters of " << x << ":" << endl;

	for (int i = 0; i<op.size(); i++) {
		cout << op[i] << endl;
		outfile << op[i] << endl;
	}

	cout << "output also saved on " << x << ".txt" << endl;

	cout << "And file" << x << ".txt is created with content:";
	for (int i = 0; i<op.size(); i++) {
		cout << op[i] << endl;
	}

	infile.close();
	outfile.close();

	return 0;
}

void allsamec(const vector<string>& a, const string& s, vector<string>& opp) {

	for (int i = 0; i<a.size(); i++) {
		string wi = a[i];
		if (havesamec(wi, s) == true) {
			opp.push_back(wi);
		}
	}
}

bool havesamec(const string& A, const string& S) {

	string word1;

	/// following two loops do the same thing of filtering out duplicate chars. Could have made a function for that.
	word1.push_back(A[0]);
	for (int i = 1; i<A.size(); i++) {
		if (A[i] != A[i - 1]) {
			word1.push_back(A[i]);
		}
	}

	string word2;
	word2.push_back(S[0]);
	for (int i = 1; i<S.size(); i++) {
		if (S[i] != S[i - 1]) {
			word2.push_back(S[i]);
		}
	}

	/// it's a bit overcomplicated; could have broken down into smaller bits
	/// sorting the arrays would immediately reveal if arrays are same
	if (word1.size() == word2.size()) {

		int count = 0;

		for (int i = 0; i<word1.size(); i++) {
			int j = 0;

			while (!(j<word2.size()) && (count == 0)) {
				if (A[i] == S[j]) {
					count = 1;;
				}

				else {
					j++;
				}
			}

			if (count == 0) {
				return false;
			}
		}

		if (count == 1) {
			return true;
		}

	}

	else {
		return false;
	}
	/// should have a return here
}

/// some sort of attempt but doesn't really work. Nothing wrong with approach
