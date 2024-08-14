#include <iostream>
#include <fstream>
#include <iomanip>
#include <locale.h>
#include <cstdlib>

using namespace std;

typedef double mytype;

const int prec = 7;

void show_column (const int a, const mytype* c) //Вывод столбца
{
	cout << endl;
	for (int i = 0; i < a; ++i)
	{
		cout.precision(prec); cout << setw(25) << c[i];
		cout << endl;
	}
}

void show_equation(const int a, const mytype* c,const mytype* d)
{
	cout << endl;
	for (int i = 0; i < a; ++i)
	{
		for (int j = 0; j < a; ++j)
		{
			cout.precision(prec); cout << setw(15) << c[i * a + j];
		}
		cout << setw(12) << "="; cout.precision(prec);cout << setw(12) << d[i] << endl;
	}
}

void show_matrix(const int a, const mytype* c)
{
	cout << endl;
	for (int i = 0; i < a; ++i)
	{
		for (int j = 0; j < a; ++j)
		{
			cout.precision(prec); cout << setw(15) << c[i * a + j];
		}	
		cout << endl;
	}
}

void triangle_matrix(const int size, mytype* A, mytype* b)
{
	mytype c;
	for (int i = 1; i < size; ++i)
	{
		mytype temp1 = fabs(A[(i - 1) * size + (i - 1)]);
		int temp2 = i - 1;
		int temp3 = temp2;
		mytype temp4;
		for (int i1 = i; i1 < size; ++i1) //Нахождение максимального по модулю числа в столбце
		{
			temp1 = (fabs(A[i1 * size + (i - 1)]) >= temp1) ? (A[i1 * size + (i - 1)]) : temp1;// значение элемента
			temp3 = (fabs(A[i1 * size + (i - 1)]) >= temp1) ? i1 : temp3;//Номер строки max элемента
		}

		if (temp3 != temp2)//Замена строк, если максимум в строке не на диагонали
		{
			for (int i2 = 0; i2 < size; ++i2)
			{
				temp1 = A[temp2 * size + i2];
				A[temp2 * size + i2] = A[temp3 * size + i2];
				A[temp3 * size + i2] = temp1;
			}
			temp1 = b[temp3];
			b[temp3] = b[temp2];
			b[temp2] = temp1;
		}

		for (int i3 = i; i3 < size; ++i3)//Исключение хi из остальных строк, приведение к треугольному виду
		{
			c = A[i3 * size + (i - 1)] / A[(i - 1) * size + (i - 1)];//коэффициент
			A[i3 * size + (i - 1)] = (mytype)0; //Нули в треугольнике снизу
			for (int i4 = i; i4 < size; ++i4)
			{
				temp1 = A[i3 * size + i4];
				A[i3 * size + i4] = temp1 - c * A[(i - 1) * size + i4];
			}
			temp1 = b[i3];
			b[i3] = temp1 - c * b[(i - 1)];
		}
	}
}

void solve_equation(const int size, const mytype* A, const mytype* b, mytype* x)
{
	x[size - 1] = b[size - 1] / A[(size - 1) * size + (size - 1)];
	for (int i = 1; i < size; ++i)
	{
		mytype sum = (mytype)0;
		for (int j = size - 1; (j >= (size - i)); --j)
			sum += A[(size - i - 1) * size + (j)] * x[j];

		x[size - i - 1] = (b[size - i - 1] - sum) / A[(size - i - 1) * size + (size - i - 1)];
	}
}

void inverse_matrix(const int size, const mytype* A, mytype* A_inv)
{
	mytype* e = new mytype [size];
	mytype* xx = new mytype[size];
	mytype* A_temp = new mytype[size*size];
	for (int i = 0; i < size; ++i)
	{
		for (int i1 = 0; i1 < size; ++i1)
			e[i1] = (i1 == i) ? (mytype)1 : (mytype)0;
		for (int i = 0; i < size; ++i)
		{
			for (int j = 0; j < size; ++j)
			{
				A_temp[i * size + j] = A[i * size + j];
			}
		}
		triangle_matrix(size, A_temp, e);
		solve_equation(size, A_temp, e, xx);
		for (int j = 0; j < size; ++j)
			A_inv[j * size + i] = xx[j];
	}
}

void multiplicate_matrix(const int size, const mytype* A, const mytype* B, mytype* C)
{
	for (int i = 0; i < size; ++i)
	{
		for (int j = 0; j < size; ++j)
		{
			mytype sum = (mytype)0;
			for (int k = 0; k < size; ++k)
			{
				sum += A[i * size + k] * B[k * size + j];
			}
			C[i * size + j] = sum;
		}
	}
}

mytype norma_1(const int size, const mytype* b) // Сумма всех элементов
{
	mytype sum = (mytype)0;
	for (int i = 0; i < size; ++i)
	{
		sum += fabs(b[i]);
	}
	return sum;
}

mytype norma_inf(const int size, const mytype* b) // (Норма беск.)Максимальное значение среди элементов
{
	mytype temp = mytype(0);
	for (int i = 0; i < size; ++i)
	{
		temp = (fabs(b[i]) > temp) ? fabs(b[i]) : temp;
	}
	return temp;
}

mytype norma_matr_1(const int size, const mytype* A)
{
	mytype sum = (mytype)0;
	mytype sumtemp = (mytype)0;
	for (int j = 0; j < size; ++j)
	{
		mytype sum = (mytype)0;
		for (int i = 0; i < size; ++i)
		{
			sum += fabs(A[i*size+j]);
		}
		sumtemp = (sum > sumtemp) ? sum : sumtemp;
	}
	
	return sumtemp;
}

mytype norma_matr_inf(const int size, const mytype* A)
{
	mytype sum = (mytype)0;
	mytype sumtemp = (mytype)0;
	for (int i = 0; i < size; ++i)
	{
		mytype sum = (mytype)0;
		for (int j = 0; j < size; ++j)
		{
			sum += fabs(A[i * size + j]);
		}
		sumtemp = (sum > sumtemp) ? sum : sumtemp;
	}

	return sumtemp;
}


int main(int argc, char** argv)
{
	setlocale(LC_ALL, "Russian");
	//Считывание данных из файла
	int size;
	ifstream infile;
	infile.open ("D4.TXT");//  P_DAT7.TXT  DATA7.TXT   D1.TXT  D2.TXT  D3.TXT  D4.TXT  D5.TXT
	infile >> size;
	cout << "Ранг матрицы " << size << endl; cout << endl;
	mytype* A_original = new mytype [size * size];//Объявление массива оформленного в виде строки
	mytype* A = new mytype[size * size]; // выделен под хранение оригинальной матрицы
	mytype* b_original = new mytype[size];//Объявление массива
	mytype* b = new mytype [size];//выделен под хранение оригинального столбца
	cout << "Оригинал матрицы" << endl; cout << endl;
	for (int i = 0; i < size; ++i)
	{
		for (int j = 0; j < size; ++j)
		{
			infile >> A_original[i * size + j];
			A[i * size + j]= A_original[i * size + j];
		}
		infile >> b_original[i];
		b[i]= b_original[i];
	}
	infile.close();
	show_equation(size, A, b);
	//Конец считывания данных из файла
	
	//Реализация м-да Гаусса
	triangle_matrix(size, A, b);
	
	//Вывод результатов перетасовки матрицы
	cout << endl;
	cout << "Измененная матрица" << endl; cout << endl;
	show_equation(size, A, b);
	cout << endl;

	//Определение определителя и единственности решения
	mytype det = (mytype)1;
	mytype tempdet;
	mytype* x = new mytype[size];
	for (int i = 0; i < size; ++i)
	{
		tempdet = A[i * size + i];
		det *= tempdet;
	}
	if (det == (mytype)0)
		cout << "Определитель = 0" << endl;
	else
	{
		cout << "Определитель не равен нулю. Существует только одно решение системы" << endl;
		
		// Решение уравнения

		solve_equation( size, A, b,  x);

		//Вывод столбца решений слау
		show_column (size,x);

		//Подстановка и вычисление невязки
		mytype* b1 = new mytype[size];
		for (int i = 0; i < size; ++i)// Подстановка в уравнение
		{
			b1[i] = (mytype)0;
			for (int j = 0; j < size; ++j)
			{
				b1[i] += A[i * size + j] * x[j];
			}
			//Получили столбец после подстановки и тут же преобразуем его к невязке, чтобы не 
			//создавать еще один массив
			b1[i] -= b[i];		
		}
		mytype norma = (mytype)0;
		norma = norma_1(size, b1);
		cout << "Невязка" << endl;
		show_column(size, b1);
		cout << endl; cout << "Норма невязки = " << norma << endl;
		delete[] b1;

		//Введение возмущения
		mytype* b_edited = new mytype[size];
		for (int i = 0; i < size; ++i) // Исходный столбец + возмущение
		{
			b_edited[i] = b[i] + (mytype)0.01;
		}
		mytype* x_edited = new mytype[size];
		solve_equation(size, A, b_edited, x_edited);
		cout << endl;
		cout << "Решение системы с возмущением " << endl;
		show_column(size, x_edited);
		for (int i = 0; i < size; ++i)// Столбец только возмущений дельта b
		{
			b_edited[i] = (mytype)0.01;
		}
		for (int i = 0; i < size; ++i)// Столбец дельта х
		{
			x_edited[i] = fabs(x_edited[i]-x[i]);
		}
		cout << endl;
		cout << "Оценка числа обусловленности снизу " << ((norma_1(size, x_edited)) / (norma_1(size, x))) / ((norma_1(size, b_edited)) / (norma_1(size, b))) << endl;
		delete[] b_edited;
		delete[] x_edited;

		// Поиск обратной матрицы
		mytype* A_inv = new mytype[size * size];
		inverse_matrix(size, A_original, A_inv);
		cout << endl;
		cout << "A^-1 = ";
		show_matrix(size, A_inv);
		mytype* E = new mytype[size * size];
		multiplicate_matrix(size, A_original, A_inv, E);
		cout << endl;
		cout << "A * A^-1 =";
		show_matrix(size, E);
		mytype obusl_1 = norma_matr_1(size, A_original) * norma_matr_1(size , A_inv);
		cout << "Число обусловленности(1) (произведение норм матриц оригинала и обратной) " << obusl_1<< endl;
		mytype obusl_inf = norma_matr_inf(size, A_original) * norma_matr_inf(size, A_inv);
		cout << "Число обусловленности(inf) (произведение норм матриц оригинала и обратной) " << obusl_inf << endl;
		delete[] A_inv;
		delete[] E;
	}
		
	// Удаление данных из кучи
	delete[] A;
	delete[] b;
	delete[] A_original;
	delete[] b_original;
	delete[] x;
	return 0;
}