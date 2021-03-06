#ifndef _INC_GLOBAL_STRUCTURES_H
#define _INC_GLOBAL_STRUCTURES_H

#include "phrqtype.h"
typedef enum { kcal, cal, kjoules, joules } DELTA_H_UNIT;
enum SURFACE_TYPE
{ UNKNOWN_DL, NO_EDL, DDL, CD_MUSIC, CCM };
enum DIFFUSE_LAYER_TYPE
{ NO_DL, BORKOVEK_DL, DONNAN_DL };
enum SITES_UNITS
{ SITES_ABSOLUTE, SITES_DENSITY };
enum entity_type
{ Solution, Reaction, Exchange, Surface, Gas_phase, Pure_phase, Ss_phase,
	Kinetics, Mix, Temperature, UnKnown
};
/* must be defined here and in cl.c */
/* #include <nan.h> */
#ifndef NAN
#   define NAN -99999999
#endif
#define RLIB_MISSING -9999.999
/* search.h -- declarations for POSIX/SVID-compatible search functions */



/* ----------------------------------------------------------------------
 *   DEFINITIONS
 * ---------------------------------------------------------------------- */
#define F_C_MOL 96493.5			/* C/mol or joule/volt-eq */
#define F_KJ_V_EQ  96.4935		/* kJ/volt-eq */
#define F_KCAL_V_EQ 23.0623		/* kcal/volt-eq */
#define R_LITER_ATM 0.0820597	/* L-atm/deg-mol */
#define R_KCAL_DEG_MOL 0.00198726	/* kcal/deg-mol */
#define R_KJ_DEG_MOL 0.00831470	/* kJ/deg-mol */
#define EPSILON 78.5			/* dialectric constant, dimensionless */
#define EPSILON_ZERO 8.854e-12	/* permittivity of free space, C/V-m = C**2/m-J */
#define JOULES_PER_CALORIE 4.1840
#define AVOGADRO 6.02252e23		/* atoms / mole */


#define TRUE 1
#define FALSE 0
#define OK 1
#define RLIB_ERROR 0
#define STOP 1
#define CONTINUE 0

#define DISP 2
#define STAG 3
#define NOMIX 4

#define CONVERGED 2
#define MASS_BALANCE 3
/*
  #define OSCILLATE 4
  #define H2O_LIMITS 5
*/
#define REWRITE 2
#define INIT -1

/* check_line values, plus EMPTY, EOF, OK */
#define KEYWORD 3

/* copy_token values */
#define EMPTY 2
#define UPPER 4
#define LOWER 5
#define DIGIT 6
#define UNKNOWN 7
#define OPTION 8

/* species types */
#define AQ 0
#define HPLUS 1
#define H2O 2
#define EMINUS 3
#define SOLID 4
#define EX 5
#define SURF 6
#define SURF_PSI 7
#define SURF_PSI1 8
#define SURF_PSI2 9

/* unknown types */
#define MB 10
#define ALK 11
#define CB 12
#define SOLUTION_PHASE_BOUNDARY 13
#define MU 14
#define AH2O 15
#define MH 16
#define MH2O 17
#define PP 18
#define EXCH 19
#define SURFACE 20
#define SURFACE_CB 21
#define SURFACE_CB1 22
#define SURFACE_CB2 23
#define GAS_MOLES 24
#define S_S_MOLES 25
#define PITZER_GAMMA 26
/* state */
#define INITIALIZE	       0
#define INITIAL_SOLUTION   1
#define INITIAL_EXCHANGE   2
#define INITIAL_SURFACE 3
#define INITIAL_GAS_PHASE  4
#define REACTION		   5
#define INVERSE		 6
#define ADVECTION		 7
#define TRANSPORT		 8
#define PHAST		     9

/* constaints in mass balance */
#define EITHER 0
#define DISSOLVE 1
#define PRECIPITATE -1

/* gas phase type */
#define PRESSURE 1
#define VOLUME 2

#define MAX_PP_ASSEMBLAGE 10	/* default estimate of the number of phase assemblages */
#define MAX_ADD_EQUATIONS 20	/* maximum number of equations added together to reduce eqn to
								   master species */
#define MAX_ELEMENTS 50			/* default estimate of the number of elements */
#define MAX_LENGTH 256			/* maximum number of characters component name */
#define MAX_LINE 4096			/* estimate of maximum line length */
#define MAX_LM 3.0				/* maximum log molality allowed in intermediate iterations */
#define MIN_LM -30.0			/* minimum log molality allowed before molality set to zero */
#define MAX_MASS_BALANCE 10		/* initial guess of number mass balance equations for a solution */
#define MAX_MASTER 50			/* default estimate of the number of master species */
#define MAX_ELTS 15				/* default estimate for maximum number of times elements occur in
								   an equation */
#define MAX_PHASES 500			/* initial guess of number of phases defined */
#define MAX_SOLUTION 10			/* The maximum number of solutions allowed */
#define MAX_S 500				/* default estimate for maximum number of species in aqueous model */
#define MAX_STRINGS 3000
#define MAX_SUM_JACOB0 50		/* list used to calculate jacobian */
#define MAX_SUM_JACOB1 500		/* list used to calculate jacobian */
#define MAX_SUM_JACOB2 500		/* list used to calculate jacobian */
#define MAX_SUM_MB 500			/* list used to calculate mass balance sums */
#define MAX_TRXN 16				/* default estimate for maximum number of components in an eqn */
#define MAX_UNKNOWNS 15			/* default estimate for maximum number of unknowns in model */
#define TOL 1e-9				/* tolerance for comparisons of double numbers */
#define LOG_ZERO_MOLALITY -30	/* molalities <= LOG_ZERO_MOLALITY are considered equal to zero */
#define MIN_TOTAL 1e-25
#define MIN_TOTAL_SS MIN_TOTAL
#define MIN_RELATED_SURFACE MIN_TOTAL*100
#define MIN_RELATED_LOG_ACTIVITY -30


/*
 *   Hash definitions
 */
/*
** Constants
*/

# define SegmentSize		    256
# define SegmentSizeShift	  8	/* log2(SegmentSize) */
# define DirectorySize	    256
# define DirectorySizeShift      8	/* log2(DirectorySize)  */
# define Prime1			  37
# define Prime2			  1048583
# define DefaultMaxLoadFactor   5

/* HSEARCH(3C) */
typedef struct entry
{
	char *key;
	void *data;
} ENTRY;
typedef enum
{ FIND, ENTER } ACTION;

/* TSEARCH(3C) */
typedef enum
{ preorder, postorder, endorder, leaf } VISIT;

typedef struct Element
{
	/*
	 ** The user only sees the first two fields,
	 ** as we pretend to pass back only a pointer to ENTRY.
	 ** {S}he doesn't know what else is in here.
	 */
	char *Key;
	char *Data;
	struct Element *Next;		/* secret from user    */
} Element, *Segment;

typedef struct
{
	short p;					/* Next bucket to be split      */
	short maxp;					/* upper bound on p during expansion */
	long KeyCount;				/* current # keys       */
	short SegmentCount;			/* current # segments   */
	short MinLoadFactor;
	short MaxLoadFactor;
	Segment *Directory[DirectorySize];
} HashTable;

typedef unsigned long Address;

typedef struct PHRQMemHeader
{
	struct PHRQMemHeader *pNext;	/* memory allocated just after this one */
	struct PHRQMemHeader *pPrev;	/* memory allocated just prior to this one */
	size_t size;				/* memory request + sizeof(PHRQMemHeader) */
#if !defined(NDEBUG)
	char *szFileName;			/* file name */
	int nLine;					/* line number */
	int dummy;					/* alignment */
#endif
} PHRQMemHeader;

struct model
{
	int force_prep;
	LDBLE temperature;
	int count_exchange;
	struct master **exchange;

	int count_kinetics;
	struct kinetics *kinetics;

	int count_gas_phase;
	struct phase **gas_phase;

	int count_s_s_assemblage;
	char **s_s_assemblage;

	int count_pp_assemblage;
	struct phase **pp_assemblage;
	char **add_formula;
	LDBLE *si;

	/*int diffuse_layer; */
	/*int edl; */
	enum DIFFUSE_LAYER_TYPE dl_type;
	enum SURFACE_TYPE surface_type;
	int only_counter_ions;
	/*int donnan; */
	LDBLE thickness;
	int count_surface_comp;
	char **surface_comp;
	int count_surface_charge;
	char **surface_charge;
};

struct name_master
{
	char *name;
	struct master *master;
};
struct name_species
{
	char *name;
	struct species *s;
};
struct name_phase
{
	char *name;
	struct phase *phase;
};
struct punch
{
	int in;
	int new_def;
	struct name_master *totals;
	int count_totals;
	struct name_species *molalities;
	int count_molalities;
	struct name_species *activities;
	int count_activities;
	struct name_phase *pure_phases;
	int count_pure_phases;
	struct name_phase *si;
	int count_si;
	struct name_phase *gases;
	int count_gases;
	struct name_phase *s_s;
	int count_s_s;
	struct name_phase *kinetics;
	int count_kinetics;
	struct name_master *isotopes;
	int count_isotopes;
	struct name_master *calculate_values;
	int count_calculate_values;
	int inverse;
	int sim;
	int state;
	int soln;
	int dist;
	int time;
	int step;
	int rxn;
	int temp;
	int ph;
	int pe;
	int alk;
	int mu;
	int water;
	int high_precision;
	int user_punch;
	int charge_balance;
	int percent_error;
};
/* ----------------------------------------------------------------------
 *   Temperatures
 * ---------------------------------------------------------------------- */
struct temperature
{
	int n_user;
	int n_user_end;
	char *description;
	LDBLE *t;
	int count_t;
};
/* ----------------------------------------------------------------------
 *   Surface
 * --------------------------------------------------------------------- */
struct surface
{
	int n_user;
	int n_user_end;
	int new_def;
	/*int diffuse_layer; */
	/*int edl; */
	int only_counter_ions;
	/*int donnan; */
	enum DIFFUSE_LAYER_TYPE dl_type;
	enum SURFACE_TYPE type;
	enum SITES_UNITS sites_units;
	LDBLE thickness;
	LDBLE debye_lengths;
	LDBLE DDL_viscosity;		/* viscosity relative to pure water */
	LDBLE DDL_limit;			/* limits DDL water to this fraction of bulk water */
	char *description;
	int solution_equilibria;
	int n_solution;
	int count_comps;
	struct surface_comp *comps;
	int count_charge;
	struct surface_charge *charge;
	int related_phases;
	int related_rate;
	int transport;				/* transports comp's and charges if true */
};
struct surface_comp
{
	char *formula;
	struct elt_list *formula_totals;
	LDBLE formula_z;
	LDBLE moles;
	struct master *master;
	struct elt_list *totals;
	LDBLE la;
	int charge;
	LDBLE cb;
	char *phase_name;
	LDBLE phase_proportion;
	char *rate_name;
	LDBLE Dw;					/* diffusion coefficient in water, used in MCD. No transport if 0 */
};
struct surface_charge
{
	char *name;
	LDBLE specific_area;
	LDBLE grams;
	LDBLE charge_balance;
	LDBLE mass_water;
	struct elt_list *diffuse_layer_totals;
	int count_g;
	struct surface_diff_layer *g;	/* stores g and dg/dXd for each ionic charge */
	LDBLE la_psi, la_psi1, la_psi2;
	LDBLE psi, psi1, psi2;
	LDBLE capacitance[2];
	LDBLE sigma0, sigma1, sigma2, sigmaddl;
};
struct surface_diff_layer
{
	LDBLE charge;
	LDBLE g;
	LDBLE dg;
	LDBLE psi_to_z;
};

struct Change_Surf
{
	char *comp_name;
	LDBLE fraction;
	char *new_comp_name;
	LDBLE new_Dw;
	int cell_no;
	int next;
};

struct Charge_Group
{
	LDBLE z;
	LDBLE eq;
};
/* ----------------------------------------------------------------------
 *   Exchange
 * ---------------------------------------------------------------------- */
struct exchange
{
	int n_user;
	int n_user_end;
	int new_def;
	char *description;
	int solution_equilibria;
	int n_solution;
	int count_comps;
	struct exch_comp *comps;
	int related_phases;
	int related_rate;
	int pitzer_exchange_gammas;
};
struct exch_comp
{
	char *formula;
	LDBLE formula_z;
	struct elt_list *formula_totals;
	LDBLE moles;
	struct master *master;
	struct elt_list *totals;
	LDBLE la;
	LDBLE charge_balance;
	char *phase_name;
	LDBLE phase_proportion;
	char *rate_name;
};
/* ----------------------------------------------------------------------
 *   Kinetics
 * ---------------------------------------------------------------------- */
struct kinetics
{
	int n_user;
	int n_user_end;
	char *description;
	int count_comps;
	struct kinetics_comp *comps;
	int count_steps;
	LDBLE *steps;
	LDBLE step_divide;
	/*char *units; */
	struct elt_list *totals;
	int rk;
	int bad_step_max;
	int use_cvode;
	int cvode_order;
	int cvode_steps;
};
struct kinetics_comp
{
	char *rate_name;
#ifdef SKIP
	char *formula;
#endif
	struct name_coef *list;
	int count_list;
	/*    struct phase *phase; */
	LDBLE tol;
	LDBLE m;
	LDBLE initial_moles;
	LDBLE m0;
	LDBLE moles;
	int count_c_params;
	char **c_params;
	int count_d_params;
	LDBLE *d_params;
};

/*----------------------------------------------------------------------
 *   Save
 *---------------------------------------------------------------------- */
struct save_values
{
	LDBLE value;
	int count_subscripts;
	int *subscripts;
};

struct save
{
	int solution;
	int n_solution_user;
	int n_solution_user_end;
	int mix;
	int n_mix_user;
	int n_mix_user_end;
	int irrev;
	int n_irrev_user;
	int n_irrev_user_end;
	int pp_assemblage;
	int n_pp_assemblage_user;
	int n_pp_assemblage_user_end;
	int exchange;
	int n_exchange_user;
	int n_exchange_user_end;
	int kinetics;
	int n_kinetics_user;
	int n_kinetics_user_end;
	int surface;
	int n_surface_user;
	int n_surface_user_end;
	int gas_phase;
	int n_gas_phase_user;
	int n_gas_phase_user_end;
	int s_s_assemblage;
	int n_s_s_assemblage_user;
	int n_s_s_assemblage_user_end;
};
/*----------------------------------------------------------------------
 *   Use
 *---------------------------------------------------------------------- */
struct Use
{
	int solution_in;
	int n_solution_user;
	int n_solution;
	struct solution *solution_ptr;

	int pp_assemblage_in;
	int n_pp_assemblage_user;
	int n_pp_assemblage;
	struct pp_assemblage *pp_assemblage_ptr;

	int mix_in;
	int n_mix_user;
	int n_mix;
	struct mix *mix_ptr;
	int n_mix_user_orig;

	int irrev_in;
	int n_irrev_user;
	int n_irrev;
	struct irrev *irrev_ptr;

	int exchange_in;
	int n_exchange_user;
	int n_exchange;
	struct exchange *exchange_ptr;

	int kinetics_in;
	int n_kinetics_user;
	int n_kinetics;
	struct kinetics *kinetics_ptr;

	int surface_in;
	int n_surface_user;
	int n_surface;
	struct surface *surface_ptr;

	int temperature_in;
	int n_temperature_user;
	int n_temperature;
	struct temperature *temperature_ptr;

	int inverse_in;
	int n_inverse_user;
	int n_inverse;
	struct inverse *inverse_ptr;

	int gas_phase_in;
	int n_gas_phase_user;
	int n_gas_phase;
	struct gas_phase *gas_phase_ptr;

	int s_s_assemblage_in;
	int n_s_s_assemblage_user;
	int n_s_s_assemblage;
	struct s_s_assemblage *s_s_assemblage_ptr;

	int trans_in;
	int advect_in;
};
/*----------------------------------------------------------------------
 *   Copy
 *---------------------------------------------------------------------- */
struct copier
{
	int count;
	int max;
	int *n_user;
	int *start;
	int *end;
};

/*----------------------------------------------------------------------
 *   Inverse
 *---------------------------------------------------------------------- */
struct inverse
{
	int n_user;
	char *description;
	int new_def;
	int minimal;
	int range;
	int mp;
	LDBLE mp_censor;
	LDBLE range_max;
	LDBLE tolerance;
	LDBLE mp_tolerance;
	int count_uncertainties;
	LDBLE *uncertainties;
	int count_ph_uncertainties;
	LDBLE *ph_uncertainties;
#ifdef SKIP
	LDBLE *alk_uncertainties;
#endif
	LDBLE water_uncertainty;
	int mineral_water;
	int carbon;
	LDBLE *dalk_dph;
	LDBLE *dalk_dc;
	int count_solns;
	int *solns;
	int count_force_solns;
	int *force_solns;
	int count_elts;
	struct inv_elts *elts;
	int count_phases;
	struct inv_phases *phases;
	int count_master_list;
	struct master **master_list;
	int count_redox_rxns;
	int count_isotopes;
	struct inv_isotope *isotopes;
	int count_i_u;
	struct inv_isotope *i_u;
	int count_isotope_unknowns;
	struct isotope *isotope_unknowns;
	char *netpath;
	char *pat;
};
struct inv_elts
{
	char *name;
	struct master *master;
	int row;
	int count_uncertainties;
	LDBLE *uncertainties;
};
struct inv_isotope
{
	char *isotope_name;
	LDBLE isotope_number;
	char *elt_name;
	int count_uncertainties;
	LDBLE *uncertainties;
};
struct inv_phases
{
	char *name;
	struct phase *phase;
	int column;
	int constraint;
	int force;
	int count_isotopes;
	struct isotope *isotopes;
};

/*----------------------------------------------------------------------
 *   Mix
 *---------------------------------------------------------------------- */
struct mix
{
	int n_user;
	int n_user_end;
	char *description;
	int count_comps;
	struct mix_comp *comps;
};
struct mix_comp
{
	int n_solution;
	LDBLE fraction;
};

/*----------------------------------------------------------------------
 *   Irreversible reaction
 *---------------------------------------------------------------------- */
struct irrev
{
	int n_user;
	int n_user_end;
	char *description;
	struct name_coef *list;
	struct elt_list *elts;
	LDBLE *steps;
	char *units;
	int count_steps;
	int count_list;
};
struct name_coef
{
	char *name;
	LDBLE coef;
};
/*----------------------------------------------------------------------
 *   Gas phase
 *---------------------------------------------------------------------- */
struct gas_phase
{
	int n_user;
	int n_user_end;
	char *description;
	int new_def;
	int solution_equilibria;
	int n_solution;
	int type;
	LDBLE total_p;
	LDBLE total_moles;
	LDBLE volume;
	LDBLE temperature;
	int count_comps;
	struct gas_comp *comps;
};
struct gas_comp
{
	struct phase *phase;
	char *name;
	LDBLE p_read;
	LDBLE moles;
	LDBLE initial_moles;
};

/*----------------------------------------------------------------------
 *   Solid solution
 *---------------------------------------------------------------------- */
struct s_s_assemblage
{
	int n_user;
	int n_user_end;
	char *description;
	int new_def;
	/*    int type; */
	/*    int solution_equilibria; */
	/*    int n_solution; */
	int count_s_s;
	struct s_s *s_s;
};
struct s_s
{
	char *name;
	struct s_s_comp *comps;
	int count_comps;
	LDBLE total_moles;
	LDBLE dn;
	LDBLE a0, a1;
	LDBLE ag0, ag1;
	int s_s_in;
	int miscibility;
	int spinodal;
	LDBLE tk, xb1, xb2;
	int input_case;
	LDBLE p[4];
};
struct s_s_comp
{
	char *name;
	struct phase *phase;
	LDBLE initial_moles;
	LDBLE moles;
	LDBLE init_moles;
	LDBLE delta;
	LDBLE fraction_x;
	LDBLE log10_lambda;
	LDBLE log10_fraction_x;
	LDBLE dn, dnc, dnb;
};

/*----------------------------------------------------------------------
 *   Pure-phase assemblage
 *---------------------------------------------------------------------- */
 struct pp_assemblage
 {
 	int n_user;
 	int n_user_end;
 	char *description;
 	int new_def;
 	struct elt_list *next_elt;
 	int count_comps;
 	struct pure_phase *pure_phases;
 };
 struct pure_phase
 {
 	struct phase *phase;
 	char *name;
 	char *add_formula;
 	LDBLE si;
 	LDBLE moles;
 	LDBLE delta;
 	LDBLE initial_moles;
 	int force_equality;
 	int dissolve_only;
 	int precipitate_only;
};

/*----------------------------------------------------------------------
 *   Species_list
 *---------------------------------------------------------------------- */
struct species_list
{
	struct species *master_s;
	struct species *s;
	LDBLE coef;
};

/*----------------------------------------------------------------------
 *   Jacobian and Mass balance lists
 *---------------------------------------------------------------------- */
struct list0
{
	LDBLE *target;
	LDBLE coef;
};
struct list1
{
	LDBLE *source;
	LDBLE *target;
};
struct list2
{
	LDBLE *source;
	LDBLE *target;
	LDBLE coef;
};

/*----------------------------------------------------------------------
 *   Solution
 *---------------------------------------------------------------------- */
 struct solution
 {
 	int new_def;
 	int n_user;
 	int n_user_end;
 	char *description;
 	LDBLE tc;
 	LDBLE ph;
 	LDBLE solution_pe;
 	LDBLE mu;
 	LDBLE ah2o;
 	LDBLE density;
 	LDBLE total_h;
 	LDBLE total_o;
 	LDBLE cb;
 	LDBLE mass_water;
 	LDBLE total_alkalinity;
 	char *units;
 	struct pe_data *pe;
 	int default_pe;
 	struct conc *totals;
 	struct master_activity *master_activity;
 	int count_master_activity;
 	int count_isotopes;
 	struct isotope *isotopes;
 	struct master_activity *species_gamma;
 	int count_species_gamma;
 };
 struct master_activity
 {
 	char *description;
 	LDBLE la;
 };
 struct conc
 {
 	char *description;
 	/*int skip; */
 	LDBLE moles;
 	LDBLE input_conc;
 	char *units;
 	char *equation_name;
 	struct phase *phase;
 	LDBLE phase_si;
 	int n_pe;
 	char *as;
 	LDBLE gfw;
 };
 struct pe_data
 {
 	char *name;
 	struct reaction *rxn;
 };
 struct isotope
 {
 	LDBLE isotope_number;
 	char *elt_name;
 	char *isotope_name;
 	LDBLE total;
 	LDBLE ratio;
 	LDBLE ratio_uncertainty;
 	LDBLE x_ratio_uncertainty;
 	struct master *master;
 	struct master *primary;
 	LDBLE coef;					/* coefficient of element in phase */
};
struct iso
{
	char *name;
	LDBLE value;
	LDBLE uncertainty;
};
/*----------------------------------------------------------------------
 *   Transport data
 *---------------------------------------------------------------------- */
struct stag_data
{
	int count_stag;
	LDBLE exch_f;
	LDBLE th_m;
	LDBLE th_im;
};
struct cell_data
{
	LDBLE length;
	LDBLE mid_cell_x;
	LDBLE disp;
	LDBLE temp;
	LDBLE por;					/* free (uncharged) porewater porosities */
	LDBLE por_il;				/* interlayer water porosities */
	int punch;
	int print;
};

/*----------------------------------------------------------------------
 *   Keywords
 *---------------------------------------------------------------------- */
 struct key
 {
 	char *name;
 	int keycount;
 };
 struct const_key
 {
 	const char *name;
 	int keycount;
};

/*----------------------------------------------------------------------
 *   Elements
 *---------------------------------------------------------------------- */
struct element
{
	char *name;					/* element name */
	/*    int in; */
	struct master *master;
	struct master *primary;
	LDBLE gfw;
};
/*----------------------------------------------------------------------
 *   Element List
 *---------------------------------------------------------------------- */
struct elt_list
{								/* list of name and number of elements in an equation */
	struct element *elt;		/* pointer to element structure */
	LDBLE coef;					/* number of element e's in eqn */
};
/*----------------------------------------------------------------------
 *   Reaction
 *---------------------------------------------------------------------- */
struct reaction
{
	LDBLE logk[8];
	LDBLE dz[3];
	struct rxn_token *token;
};
struct rxn_token
{
	struct species *s;
	LDBLE coef;
	char *name;
};
/*----------------------------------------------------------------------
 *   Species
 *---------------------------------------------------------------------- */
struct species
{								/* all data pertinent to an aqueous species */
	char *name;					/* name of species */
	char *mole_balance;			/* formula for mole balance */
	int in;						/* species used in model if TRUE */
	int number;
	struct master *primary;		/* points to master species list, NULL if not primary master */
	struct master *secondary;	/* points to master species list, NULL if not secondary master */
	LDBLE gfw;					/* gram formula wt of species */
	LDBLE z;					/* charge of species */
	LDBLE dw;					/* tracer diffusion coefficient in water at 25oC, m2/s */
	LDBLE erm_ddl;				/* enrichment factor in DDL */
	LDBLE equiv;				/* equivalents in exchange species */
	LDBLE alk;					/* alkalinity of species, used for cec in exchange */
	LDBLE carbon;				/* stoichiometric coefficient of carbon in species */
	LDBLE co2;					/* stoichiometric coefficient of C(4) in species */
	LDBLE h;					/* stoichiometric coefficient of H in species */
	LDBLE o;					/* stoichiometric coefficient of O in species */
	LDBLE dha, dhb, a_f;		/* WATEQ Debye Huckel a and b-dot; active_fraction coef for exchange species */
	LDBLE lk;					/* log10 k at working temperature */
	LDBLE logk[8];				/* log kt0, delh, 6 coefficients analalytical expression */
/* VP: Density Start */
	LDBLE millero[6];		    /* regression coefficients to calculate temperature dependent phi_0 and b_v of Millero density model */
/* VP: Density End */
	DELTA_H_UNIT original_units;	/* enum with original delta H units */
	int count_add_logk;
	struct name_coef *add_logk;
	LDBLE lg;					/* log10 activity coefficient, gamma */
	LDBLE lg_pitzer;			/* log10 activity coefficient, from pitzer calculation */
	LDBLE lm;					/* log10 molality */
	LDBLE la;					/* log10 activity */
	LDBLE dg;					/* gamma term for jacobian */
	LDBLE dg_total_g;
	LDBLE moles;				/* moles in solution; moles/mass_water = molality */
	int type;					/* flag indicating presence in model and types of equations */
	int gflag;					/* flag for preferred activity coef eqn */
	int exch_gflag;				/* flag for preferred activity coef eqn */
	struct elt_list *next_elt;	/* pointer to next element */
	struct elt_list *next_secondary;
	struct elt_list *next_sys_total;
	int check_equation;			/* switch to check equation for charge and element balance */
	struct reaction *rxn;		/* pointer to data base reaction */
	struct reaction *rxn_s;		/* pointer to reaction converted to secondary and primary
								   master species */
	struct reaction *rxn_x;		/* reaction to be used in model */
	LDBLE tot_g_moles;			/* (1 + sum(g)) * moles */
	LDBLE tot_dh2o_moles;		/* sum(moles*g*Ws/Waq) */
	struct species_diff_layer *diff_layer;	/* information related to diffuse layer factors for each
											   surface */
	LDBLE cd_music[5];
	LDBLE dz[3];
};
struct logk
{								/* Named log K's */
	char *name;					/* name of species */
	LDBLE lk;					/* log10 k at working temperature */
	LDBLE log_k[8];				/* log kt0, delh, 6 coefficients analalytical expression */
	DELTA_H_UNIT original_units;	/* enum with original delta H units */
	int count_add_logk;
	int done;
	struct name_coef *add_logk;
	LDBLE log_k_original[8];	/* log kt0, delh, 5 coefficients analalytical expression */
};
struct species_diff_layer
{
	struct surface_charge *charge;
	int count_g;
	LDBLE g_moles;
	LDBLE dg_g_moles;			/* g_moles*dgterm */
	LDBLE dx_moles;
	LDBLE dh2o_moles;			/* moles*g*Ws/Waq */
	LDBLE drelated_moles;		/* for related phase */
};

/*----------------------------------------------------------------------
 *   Phases
 *---------------------------------------------------------------------- */
struct phase
{								/* all data pertinent to a pure solid phase */
	char *name;					/* name of species */
	char *formula;				/* chemical formula */
	int in;						/* species used in model if TRUE */
	LDBLE lk;					/* log10 k at working temperature */
	LDBLE logk[8];				/* log kt0, delh, 6 coefficients analalytical expression */
	DELTA_H_UNIT original_units;	/* enum with original delta H units */
	int count_add_logk;
	struct name_coef *add_logk;
	LDBLE moles_x;
	LDBLE p_soln_x;
	LDBLE fraction_x;
	LDBLE log10_lambda, log10_fraction_x;
	LDBLE dn, dnb, dnc;
	LDBLE gn, gntot;
	LDBLE gn_n, gntot_n;

	int type;					/* flag indicating presence in model and types of equations */
	struct elt_list *next_elt;	/* pointer to list of elements in phase */
	struct elt_list *next_sys_total;
	int check_equation;			/* switch to check equation for charge and element balance */
	struct reaction *rxn;		/* pointer to data base reaction */
	struct reaction *rxn_s;		/* pointer to reaction converted to secondary and primary
								   master species */
	struct reaction *rxn_x;		/* reaction to be used in model */
	int replaced;               /* equation contains solids or gases */
	int in_system;
};
/*----------------------------------------------------------------------
 *   Master species
 *---------------------------------------------------------------------- */
 struct master
 {								/* list of name and number of elements in an equation */
 	int in;						/* TRUE if in model, FALSE if out, REWRITE if other mb eq */
 	int number;					/* sequence number in list of masters */
 	int last_model;				/* saved to determine if model has changed */
 	int type;					/* AQ or EX */
 	int primary;				/* TRUE if master species is primary */
 	LDBLE coef;					/* coefficient of element in master species */
 	LDBLE total;				/* total concentration for element or valence state */
 	LDBLE isotope_ratio;
 	LDBLE isotope_ratio_uncertainty;
 	int isotope;
 	LDBLE total_primary;
 	/*    LDBLE la;  */ /* initial guess of master species log activity */
 	struct element *elt;		/* element structure */
 	LDBLE alk;					/* alkalinity of species */
 	LDBLE gfw;					/* default gfw for species */
 	char *gfw_formula;			/* formula from which to calcuate gfw */
 	struct unknown *unknown;	/* pointer to unknown structure */
 	struct species *s;			/* pointer to species structure */
 	struct reaction *rxn_primary;	/* reaction writes master species in terms of primary
 									   master species */
 	struct reaction *rxn_secondary;	/* reaction writes master species in terms of secondary
 									   master species */
 	struct reaction **pe_rxn;	/* e- written in terms of redox couple (or e-), points
 								   to location */
 	int minor_isotope;
};
/*----------------------------------------------------------------------
 *   Unknowns
 *---------------------------------------------------------------------- */
struct unknown
{
	int type;
	LDBLE moles;
	LDBLE ln_moles;
	LDBLE f;
	LDBLE sum;
	LDBLE delta;
	LDBLE la;
	int number;
	char *description;
	struct master **master;
	struct phase *phase;
	LDBLE si;
	struct gas_phase *gas_phase;
	struct conc *total;
	struct species *s;
	struct exch_comp *exch_comp;
	struct pure_phase *pure_phase;
	struct s_s *s_s;
	struct s_s_comp *s_s_comp;
	int s_s_comp_number;
	int s_s_in;
	struct surface_comp *surface_comp;
	LDBLE related_moles;
	struct unknown *potential_unknown, *potential_unknown1,
		*potential_unknown2;
	int count_comp_unknowns;
	struct unknown **comp_unknowns;	/* list for CD_MUSIC of comps that contribute to 0 plane mass-balance term */
	struct unknown *phase_unknown;
	struct surface_charge *surface_charge;
	LDBLE mass_water;
	int dissolve_only;
	LDBLE inert_moles;
};

/*----------------------------------------------------------------------
 *   Reaction work space
 *---------------------------------------------------------------------- */
struct reaction_temp
{
	LDBLE logk[8];
	LDBLE dz[3];
	struct rxn_token_temp *token;
};
struct rxn_token_temp
{								/* data for equations, aq. species or minerals */
	char *name;					/* pointer to a species name (formula) */
	LDBLE z;					/* charge on species */
	struct species *s;
	struct unknown *unknown;
	LDBLE coef;					/* coefficient of species name */
};
struct unknown_list
{
	struct unknown *unknown;
	LDBLE *source;
	LDBLE *gamma_source;
	/*    int row; */
	/*    int col; */
	LDBLE coef;
};
/* ----------------------------------------------------------------------
 *   Print
 * ---------------------------------------------------------------------- */
struct prints
{
	int all;
	int initial_solutions;
	int initial_exchangers;
	int reactions;
	int gas_phase;
	int s_s_assemblage;
	int pp_assemblage;
	int surface;
	int exchange;
	int kinetics;
	int totals;
	int eh;
	int species;
	int saturation_indices;
	int irrev;
	int mix;
	int reaction;
	int use;
	int logfile;
	int punch;
	int status;
	int inverse;
	int dump;
	int user_print;
	int headings;
	int user_graph;
	int echo_input;
	int warnings;
	int initial_isotopes;
	int isotope_ratios;
	int isotope_alphas;
	int hdf;
	int alkalinity;
};
/* ----------------------------------------------------------------------
 *   RATES
 * ---------------------------------------------------------------------- */
struct rate
{
	char *name;
	char *commands;
	int new_def;
	void *linebase;
	void *varbase;
	void *loopbase;
};
/* ----------------------------------------------------------------------
 *   GLOBAL DECLARATIONS
 * ---------------------------------------------------------------------- */
struct spread_row
{
	int count;
	int empty, string, number;
	char **char_vector;
	LDBLE *d_vector;
	int *type_vector;
};
struct defaults
{
	LDBLE temp;
	LDBLE density;
	char *units;
	char *redox;
	LDBLE ph;
	LDBLE pe;
	LDBLE water;
	int count_iso;
	struct iso *iso;
};
struct spread_sheet
{
	struct spread_row *heading;
	struct spread_row *units;
	int count_rows;
	struct spread_row **rows;
	struct defaults defaults;
};
/* ----------------------------------------------------------------------
 *   ISOTOPES
 * ---------------------------------------------------------------------- */
struct master_isotope
{
	char *name;
	struct master *master;
	struct element *elt;
	char *units;
	LDBLE standard;
	LDBLE ratio;
	LDBLE moles;
	int total_is_major;
	int minor_isotope;
};
struct calculate_value
{
	char *name;
	LDBLE value;
	char *commands;
	int new_def;
	int calculated;
	void *linebase;
	void *varbase;
	void *loopbase;
};
struct isotope_ratio
{
	char *name;
	char *isotope_name;
	LDBLE ratio;
	LDBLE converted_ratio;
};
struct isotope_alpha
{
	char *name;
	char *named_logk;
	LDBLE value;
};
/* ----------------------------------------------------------------------
 *   System
 * ---------------------------------------------------------------------- */
struct system
{
	struct solution *solution;
	struct exchange *exchange;
	struct pp_assemblage *pp_assemblage;
	struct gas_phase *gas_phase;
	struct s_s_assemblage *s_s_assemblage;
	struct kinetics *kinetics;
	struct surface *surface;
};
struct system_species
{
	char *name;
	char *type;
	LDBLE moles;
};
#if defined(PHREEQC_CLASS)
/* tally.c ------------------------------- */
struct tally_buffer
{
	char *name;
	struct master *master;
	LDBLE moles;
	LDBLE gfw;
};
struct tally
{
	char *name;
	enum entity_type type;
	char *add_formula;
	LDBLE moles;
	struct elt_list *formula;
	/*
	 * first total is initial
	 * second total is final
	 * third total is difference (final - initial)
	 */
	struct tally_buffer *total[3];
};

/* transport.c ------------------------------- */
struct spec
{
	char *name;					/* name of species */
	char *aq_name;				/* name of aqueous species in EX species */
	int type;					/* type: AQ or EX */
	LDBLE a;					/* activity */
	LDBLE lm;					/* log(concentration) */
	LDBLE lg;					/* log(gamma) */
	LDBLE c;					/* concentration for AQ, equivalent fraction for EX */
	LDBLE z;					/* charge number */
	LDBLE Dwt;					/* temperature corrected free water diffusion coefficient, m2/s */
	LDBLE erm_ddl;				/* enrichment factor in ddl */
};
struct sol_D
{
	int count_spec;				/* number of aqueous + exchange species */
	int count_exch_spec;		/* number of exchange species */
	LDBLE exch_total;			/* total moles of X- */
	struct spec *spec;
};
struct J_ij
{
	char *name;
	LDBLE tot1, tot2;
};
struct M_S
{
	char *name;
	LDBLE tot1, tot2;
};
/* basic.c ------------------------------- */
#ifdef PHREEQ98
int colnr, rownr;
#endif

#define checking	true
#define varnamelen  20
#define maxdims	    4


#include "basic.h"


#endif /* PHREEQC_CLASS) */
#endif /* _INC_GLOBAL_STRUCTURES_H  */

