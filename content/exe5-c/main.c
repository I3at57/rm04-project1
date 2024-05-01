#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include <time.h>

//////////////////////////////
/* Personal struct */
//////////////////////////////

typedef struct {
    double horizon;
    int nbr_component;
    double shape;
    double scale;
    double x;
    int cmp;
    int cmc;
    int policy;
} Parameters;

//////////////////////////////
/* Personal math function */
//////////////////////////////

double find_min_array(double array[], int size) {
    double min = array[0]; // Assume the first element is the minimum

    // Loop through the array to find the minimum value
    for (int i = 1; i < size; i++) {
        if (array[i] < min) {
            min = array[i]; // Update min if a smaller value is found
        }
    }
    return min;
}

double find_min_comparation(double a, double b) {
    if (a <= b) {
        return(a);
    } else {
        return(b);
    }
}

//////////////////////////////
/* Personal probabilities fonctions */
//////////////////////////////

int random_number(int min_num, int max_num)
{
    int result = 0, low_num = 0, hi_num = 0;

    if (min_num < max_num)
    {
        low_num = min_num;
        hi_num = max_num + 1; // include max_num in output
    } else {
        low_num = max_num + 1; // include max_num in output
        hi_num = min_num;
    }

    srand(time(NULL));
    result = (rand() % (hi_num - low_num)) + low_num;
    return result;
}

int X0 = 1; // Pour U(A,B)
int ALPHA = 15;
int BETA = 7;
int GAMMA = 15654;
double random_uniform(double A, double B)
{
	// retourne une valeur aléatoire uniformément
	// répartie entre A et B, des réels
    X0 = (ALPHA*X0+BETA)%(GAMMA);
    double val = (double) X0/GAMMA;
    return (double) val*(B-A)+A;
}

void set_seed(int x0, int alpha, int beta, int gamma) {
    X0 = x0;
    ALPHA = alpha;
    BETA = beta;
    GAMMA = gamma;
}

double random_exponential(double rate)
{
	// Retourne une valeur de loi expo pour un paramètre 
	// Lambda, donc de moyenne 1/rate
	double R=random_uniform(0,1);
	return (double) -1*logf(R)/rate;
}

double random_weibull(double scale, double shape)
{
    // return random value from weibull low
    double R = (double) scale*pow((-logf(random_uniform(0,1))),1/shape);
    return (double) R;
}

//////////////////////////////
/* Fonctions de système */
//////////////////////////////

int realisation_trajectoire_sys(Parameters param, int info)
{
    // current time
    double t = 0;
    // current total cost
    int TC = 0;
    // components date fault
    double components_fault[param.nbr_component];
    // next preventive maintenance date
    double next_preventive_maintenance_date;
    // next event
    double next_event;

    // Display
    if (info == 1)
    {
        printf("=== Debut de simulation ===\n");
    }
    
    // initialize components fault date
    for (int i = 0; i<param.nbr_component; i++) {
        components_fault[i] = t + random_weibull(param.scale, param.shape);
    }

    // initialize next preventive maintenance
    next_preventive_maintenance_date = (double) t + param.x;
    
    // Display
    if (info == 1) {
        printf("Current time : %f\n", t);
        printf("Next fault date %f\n", find_min_array(components_fault, param.nbr_component));
        for (int i = 0; i<param.nbr_component; i++) {
           printf("Component %d fault date : %f\n", i, components_fault[i]);
        }
        printf("Next preventive maintenance date : %f\n", next_preventive_maintenance_date);
        printf("Maintenance policy : %d\n", param.policy);
    }
    
    do {
        // check if the next event is a fault of a maintenance
        if (find_min_array(components_fault, param.nbr_component)<next_preventive_maintenance_date) {

            // In this case the next event is a component fault
            if (info == 1) {printf("Corrective maintenance\n");}

            // Go to this date
            t = find_min_array(components_fault, param.nbr_component);

            // Do corrective maintenance
            TC = TC + param.cmc;
            
            if (param.policy == 1) {
                // new components fault date
                for (int i = 0; i<param.nbr_component; i++) {
                    components_fault[i] = t + random_weibull(param.scale, param.shape);
                }
                // new preventive maintenance date
                next_preventive_maintenance_date = (double) t + param.x;
            }
          
        } else {

            // In this case the next event is a preventive maintenance
            if (info == 1) {printf("Preventive maintenance\n");}

            t = (double) next_preventive_maintenance_date;
            TC = (int) TC + param.cmp;
            next_preventive_maintenance_date = (double) t + param.x;
        }
        
        if (info == 1) {
            printf("===\n");
            printf("Current time : %f\n", t);
            printf("Next fault date %f\n", find_min_array(components_fault, param.nbr_component));
            printf("Next preventive maintenance date : %f\n", next_preventive_maintenance_date);
            printf("Current cost : %d\n", TC);
        }
        
        // Find next event date 
        next_event = (double) find_min_comparation(
            next_preventive_maintenance_date,
            find_min_array(components_fault, param.nbr_component)
        );

    } while (next_event < param.horizon);
    
    return(TC);
}

double compute_cost_by_time(int p, Parameters param, int info) {
    int sum = 0;
    double simu;
    for (int i=1;i<=p;i++){
        if (i%info == 0) {
            simu = realisation_trajectoire_sys(param, 0)/param.horizon;
            printf("Simulation : %d, valeur : %f\n", i, simu);
            sum = sum + simu;
        } else {
            sum = sum + realisation_trajectoire_sys(param, 0)/param.horizon;
        }

    }

    return (double) sum/p;
}

int main()
{
    set_seed(1,48271,0,2^31-1);
    /*

    Parameters param1;
    param1.horizon = 1000.0;
    param1.x = (double) 3;
    param1.nbr_component = (int) 1;
    param1.scale = (double) 3;
    param1.shape = (double) 2.5;
    param1.cmc = (int) 1000;
    param1.cmp = (int) 200;
    param1.policy = 1;

    printf(
        "Cout moyen du systeme : %f",
        compute_cost_by_time(100000, param1, 10000)
    );
    */

    double tot = 0;
    for (int i=0;i<1000000;i++) {
        tot = tot + random_weibull(3,2.5);
    }
    printf("%f %f", tot, tot/1000000);
    
    return(0);
}