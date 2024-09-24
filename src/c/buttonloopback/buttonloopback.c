// This is the simulated version of button_state. A real
// implementation would real each button via GPIO.

#include <stdbool.h> 

#define MAX_BUTTON 60 /* number of buttons */

static bool button_states[MAX_BUTTON];   /* storage for state */

int button_state(int button_number) {
    if (button_number < 0 || button_number >= MAX_BUTTON) {
	return -1;
    } else {
	return button_states[MAX_BUTTON];
    }
}

int set_button_state(int button_number, int new_value) {
    if (button_number < 0 || button_number >= MAX_BUTTON) {
	return -1;
    } else if (new_value < 0 || new_value > 1) {
	return -2;
    } else {
	button_states[MAX_BUTTON] = (new_value == 1);
	return new_value;
    }
}

int init_buttons() {
    for (int i = 0; i < MAX_BUTTON; i++) {
	button_states[i] = false;
    }

    return 0;
}
