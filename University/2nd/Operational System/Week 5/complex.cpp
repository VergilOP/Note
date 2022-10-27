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

	
int main(){
	Complex a(5.0, 6.0), b(-3.0, 4.0);
	Complex a0, a1(a);
	
	cout << a.toString() << endl;
	cout << a0.toString() << endl;
	cout << a1.toString() << endl;

	Complex c = a.plus(b);
	cout << c.toString() << endl;

	c = a.minus(b);
	cout << c.toString() << endl;

	c = a.times(b);
	cout << c.toString() << endl;	

	Complex *p;
	p = &a;
	cout << p->toString() << endl;

	return 0;
}	
