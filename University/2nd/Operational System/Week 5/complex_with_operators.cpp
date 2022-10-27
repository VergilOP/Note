#include <iostream>
#include <string>
using namespace std;

class Complex {
		private:
    double re;   // the real part
    double im;   // the imaginary part

		public:

		// default constructor
    Complex(); 

    // create a new object with the given real and imaginary parts
    Complex(double real, double imag); 

		// copy constructor
    Complex(const Complex &c); 


    // return a string representation of the invoking Complex object
    string toString();
		
		// arithmetic functions	
		Complex plus(Complex b);
		Complex minus(Complex b);
		Complex times(Complex b);

		Complex operator+(Complex b);
		Complex operator-();
		Complex operator +(float f);
		friend Complex operator+(float f, Complex c);
		
		~Complex();
};


Complex::Complex(){
	re = 0;
  im = 0;
}

Complex::Complex(double real, double imag){
	re = real;
  im = imag;
}
	
Complex::Complex(const Complex &c){
	re = c.re;
  im = c.im;
}

Complex::~Complex(){
	cout << "Destructor called" << endl;
}
	
string Complex::toString() {
	string temp;
	if (im == 0) temp = to_string(re) + "";
	if (re == 0) temp = to_string(im) + "i";
	if (im <  0) temp = to_string(re) + " - " + to_string(-im) + "i";
	else temp = to_string(re) + " + " + to_string(im) + "i";
	return temp;
}
	
Complex Complex::plus(Complex b) {
	double real = this->re + b.re;
	double imag = this->im + b.im;
	return Complex(real, imag);
}	

Complex Complex::minus(Complex b) {
	double real = this->re - b.re;
	double imag = this->im - b.im;
	return Complex(real, imag);
}

Complex Complex::times(Complex b) {
	double real = this->re * b.re - this->im * b.im;
	double imag = this->re * b.im + this->im * b.re;
	return Complex(real, imag);
}

Complex Complex::operator +(Complex b) {
	double real = this->re + b.re;
	double imag = this->im + b.im;
	return Complex(real, imag);
}

Complex Complex::operator -() {
	double real = -this->re;
	double imag = -this->im;
	return Complex(real, imag);
}

Complex Complex::operator +(float f) {
   double real = this->re + f;
   double imag = this->im;
   return Complex(real, imag);
}
	
// A friend function is defined like an ordinary function.
// No class resolution operator is used.
Complex operator +(float f, Complex c) {
	double real = f + c.re;
	double imag = c.im;
	return Complex(real, imag);
}

	
int main(){
	Complex a(5.0, 6.0), b(-3.0, 4.0);
	Complex c;

	c = a + b;
	cout << "c=a+b = " << c.toString() << endl;

	c = -c;
	cout << "c=-c = " << c.toString() << endl;	

	c = a + 3.0;
	cout << "c=a+3.0 = " << c.toString() << endl;

	c = 3.0 + a;
	cout << "c=3.0+a = " << c.toString() << endl;
	
	return 0;
}	