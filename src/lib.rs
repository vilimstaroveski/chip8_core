use core::panic;
use rand::random;

pub const SCREEN_WIDTH: usize = 64; // screen width
pub const SCREEN_HEIGHT: usize = 32; // screen height

const RAM_SIZE: usize = 4096; // bytes
const V_REGISTERS_COUNT: usize = 16;
const STACK_SIZE: usize = 16;
const KEYBOARD_SIZE: usize = 16;
const START_ADDR: u16 = 0x200; // start from 512kB (first memory is reserved)

const FONTSET_SIZE: usize = 80;

const FONTSET: [u8; FONTSET_SIZE] = [
    0xF0, 0x90, 0x90, 0x90, 0xF0, // 0
    0x20, 0x60, 0x20, 0x20, 0x70, // 1
    0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2
    0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3
    0x90, 0x90, 0xF0, 0x10, 0x10, // 4
    0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5
    0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6
    0xF0, 0x10, 0x20, 0x40, 0x40, // 7
    0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8
    0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9
    0xF0, 0x90, 0xF0, 0x90, 0x90, // A
    0xE0, 0x90, 0xE0, 0x90, 0xE0, // B
    0xF0, 0x80, 0x80, 0x80, 0xF0, // C
    0xE0, 0x90, 0x90, 0x90, 0xE0, // D
    0xF0, 0x80, 0xF0, 0x80, 0xF0, // E
    0xF0, 0x80, 0xF0, 0x80, 0x80  // F
];

const FLAG_REGISTER_INDEX: usize = 15;

pub struct Emu {
    program_counter: u16, // program counter
    ram: [u8; RAM_SIZE], // 4kB of ram memory
    screen: [bool; SCREEN_WIDTH * SCREEN_HEIGHT], // mono-chromatic screen 64*32
    v_registers: [u8; V_REGISTERS_COUNT], // V registers
    i_register: u16, // I register
    stack: [u16; STACK_SIZE], // stack
    stack_pointer: usize, // stack pointer (points to the top of stack)
    keys: [bool; KEYBOARD_SIZE], // keyboard (tracking which keys are pressed)
    delay_timer: u8, // used by the system as a typical timer, counting down every cycle and performing some action if it hits 0
    sound_timer: u8, // counts down every clock cycle, but upon hitting 0 emits a noise
}

impl Emu {
    pub fn new() -> Self {
        let mut new_emu = Self { 
            program_counter: START_ADDR, 
            ram: [0; RAM_SIZE], 
            screen: [false; SCREEN_WIDTH * SCREEN_HEIGHT], 
            v_registers: [0; V_REGISTERS_COUNT],
            i_register: 0,
            stack: [0; STACK_SIZE],
            stack_pointer: 0,
            keys: [false; KEYBOARD_SIZE],
            delay_timer: 0,
            sound_timer: 0    
        };

        // copies FONTSET to first FONTSET_SIZE bytes of ram ([..FONTSIZE] == [0..FONTSIZE])
        new_emu.ram[..FONTSET_SIZE].copy_from_slice(&FONTSET);

        new_emu
    }

    pub fn reset(&mut self) {
        self.program_counter = START_ADDR;
        self.ram = [0; RAM_SIZE]; 
        self.screen = [false; SCREEN_WIDTH * SCREEN_HEIGHT];
        self.v_registers = [0; V_REGISTERS_COUNT];
        self.i_register = 0;
        self.stack = [0; STACK_SIZE];
        self.stack_pointer = 0;
        self.keys = [false; KEYBOARD_SIZE];
        self.delay_timer = 0;
        self.sound_timer = 0;

        self.ram[..FONTSET_SIZE].copy_from_slice(&FONTSET);
    }

    pub fn stack_push(&mut self, value: u16) -> Result<(), String> {
        if self.stack_pointer + 1 == STACK_SIZE {
            return Result::Err("Stack is full!".to_string());
        }
        self.stack_pointer += 1;
        self.stack[self.stack_pointer] = value;
        Result::Ok(())
    }

    pub fn stack_pop(&mut self) -> Result<u16, String> {
        if self.stack_pointer == 0 {
            return Result::Err("Stack is empty!".to_string());
        }
        let value = self.stack[self.stack_pointer];
        self.stack_pointer -= 1;
        Result::Ok(value)
    }

    pub fn tick(&mut self) {
        // 1) fetch opcode at which program counter is pointing to and move program counter
        let opcode = self.fetch();
        // 2) decode & execute
        self.execute(opcode);
    }

    fn fetch(&mut self) -> u16 {
        let higher_byte = self.ram[self.program_counter as usize] as u16;
        let lower_byte = self.ram[(self.program_counter + 1) as usize] as u16;
        let opcode = (higher_byte << 8) | lower_byte;
        self.program_counter += 2;
        opcode
    }

    pub fn tick_timers(&mut self) {
        if self.delay_timer > 0 {
            self.delay_timer -= 1;
        }
    
        if self.sound_timer > 0 {
            if self.sound_timer == 1 {
                // BEEP
            }
            self.sound_timer -= 1;
        }
    }

    fn execute(&mut self, opcode: u16) {
        let digit1 = (opcode & 0xF000) >> 12;
        let digit2 = (opcode & 0x0F00) >> 8;
        let digit3 = (opcode & 0x00F0) >> 4;
        let digit4 = opcode & 0x000F;


        match (digit1, digit2, digit3, digit4) {
            (0, 0, 0, 0) => return, // NOP
            (0, 0, 0xE, 0) => self.clear(),
            (0, 0, 0xE, 0xE) => self.return_from_subroutine(),
            (1, n1, n2, n3) => self.goto(Self::as_u16(n1, n2, n3)),
            (2, n1, n2, n3) => self.call_subroutine(Self::as_u16(n1, n2, n3)),
            (3, x, b, c) => self.skip_next_if_reg_equals_val(x as usize, Self::as_u8(b,c)),
            (4, x, b, c) => self.skip_next_if_reg_not_equals_val(x as usize, Self::as_u8(b,c)),
            (5, x, y, 0) => self.skip_next_if_reg_equals_reg(x as usize, y as usize),
            (6, x, n1, n2) => self.set_reg(x as usize, Self::as_u8(n1, n2)),
            (7, x, n1, n2) => self.increment_reg(x as usize, Self::as_u8(n1, n2)),
            (8, x, y, 0) => self.set_reg_to_reg(x as usize, y as usize),
            (8, x, y, 1) => self.bitwise_or(x as usize, y as usize),
            (8, x, y, 2) => self.bitwise_and(x as usize, y as usize),
            (8, x, y, 3) => self.bitwise_xor(x as usize, y as usize),
            (8, x, y, 4) => self.increment_reg_with_reg(x as usize, y as usize),
            (8, x, y, 5) => self.decrement_reg_with_reg(x as usize, y as usize),
            (8, x, _, 6) => self.right_shift_reg(x as usize),
            (8, x, y, 7) => self.decrement_reg_with_reg_opposite(x as usize, y as usize),
            (8, x, _, 0xE) => self.left_shift_reg(x as usize),
            (9, x, y, 0) => self.skip_next_if_reg_not_equals_reg(x as usize, y as usize),
            (0xA, n1, n2, n3) => self.set_i_reg(Self::as_u16(n1, n2, n3)),
            (0xB, n1, n2, n3) => self.goto_reg_0_plus_val(Self::as_u16(n1, n2, n3)),
            (0xC, x, n1, n2) => self.rand_and_val(x as usize, Self::as_u8(n1, n2)),
            (0xD, x, y, n) => self.draw_sprite(x as usize, y as usize, n),
            (0xE, x, 9, 0xE) => self.skip_if_pressed(x as usize),
            (0xE, x, 0xA, 1) => self.skip_if_not_pressed(x as usize),
            (0xF, x, 0, 7) => self.store_delay_timer(x as usize),
            (0xF, x, 0, 0xA) => self.wait_for_key_press(x as usize),
            (0xF, x, 1, 5) => self.set_delay_timer(x as usize),
            (0xF, x, 1, 8) => self.set_sound_timer(x as usize),
            (0xF, x, 1, 0xE) => self.increment_i_reg(x as usize),
            (0xF, x, 2, 9) => self.set_i_reg_to_font_address(x as usize),
            (0xF, x, 3, 3) => self.bcd_of_reg(x as usize),
            (0xF, x, 5, 5) => self.store_reg_to_ram(x as usize),
            (0xF, x, 6, 5) => self.store_ram_to_reg(x as usize),

            _ => unimplemented!("not implemented")
        }
    }

    fn as_u16(digit1: u16, digit2: u16, digit3: u16) -> u16 {
        (digit1 << 8) | (digit2 << 4) | digit3
    }

    fn as_u8(digit1: u16, digit2: u16) -> u8 {
        ((digit1 << 4) | digit2) as u8
    }

    pub fn get_display(&self) -> &[bool] {
        &self.screen
    }

    pub fn set_key_pressed(&mut self, index: usize, pressed: bool) {
        self.keys[index] = pressed;
    }

    pub fn load(&mut self, data: &[u8]) {
        let program_start = START_ADDR as usize;
        let program_end = START_ADDR as usize + data.len();
        self.ram[program_start..program_end].copy_from_slice(data);
    }


    // ************************
    // *** INSTRUCTIONS SET ***
    // ************************

    fn clear(&mut self) {
        self.screen = [false; SCREEN_WIDTH * SCREEN_HEIGHT];
    }

    fn return_from_subroutine(&mut self) {
        match self.stack_pop() {
            Ok(return_address) => self.program_counter = return_address,
            Err(message) => panic!("Error while trying to return from subroutine: {}", message)
        }
    }

    fn goto(&mut self, address: u16) {
        self.program_counter = address;
    }

    fn call_subroutine(&mut self, address: u16) {
        // add current program counter to stack (so we can return from subroutine)
        match self.stack_push(self.program_counter) {
            Ok(_) => self.program_counter = address, // set program counter to 'address'
            Err(message) => panic!("Error while trying to call subroutine: {}", message)
        }
    }

    fn skip_next_if_reg_equals_val(&mut self, register_index: usize, value: u8) {
        if self.v_registers[register_index] == value {
            self.program_counter += 2;
        }
    }

    fn skip_next_if_reg_not_equals_val(&mut self, register_index: usize, value: u8) {
        if self.v_registers[register_index] != value {
            self.program_counter += 2;
        }
    }

    fn skip_next_if_reg_equals_reg(&mut self, register_x_index: usize, register_y_index: usize) {
        if self.v_registers[register_x_index] == self.v_registers[register_y_index] {
            self.program_counter += 2;
        }
    }

    fn skip_next_if_reg_not_equals_reg(&mut self, register_x_index: usize, register_y_index: usize) {
        if self.v_registers[register_x_index] != self.v_registers[register_y_index] {
            self.program_counter += 2;
        }
    }

    fn set_reg(&mut self, register_index: usize, value: u8) {
        self.v_registers[register_index] = value;
    }

    fn increment_reg(&mut self, register_index: usize, value: u8) {
        self.v_registers[register_index] = self.v_registers[register_index].wrapping_add(value);
    }

    fn set_reg_to_reg(&mut self, register_x_index: usize, register_y_index: usize) {
        self.v_registers[register_x_index] = self.v_registers[register_y_index];
    }

    fn bitwise_or(&mut self, register_x_index: usize, register_y_index: usize) {
        self.v_registers[register_x_index] |= self.v_registers[register_y_index];
    }

    fn bitwise_and(&mut self, register_x_index: usize, register_y_index: usize) {
        self.v_registers[register_x_index] &= self.v_registers[register_y_index];
    }

    fn bitwise_xor(&mut self, register_x_index: usize, register_y_index: usize) {
        self.v_registers[register_x_index] ^= self.v_registers[register_y_index];
    }

    fn increment_reg_with_reg(&mut self, register_x_index: usize, register_y_index: usize) {
        let (sum, carry) = self.v_registers[register_x_index].overflowing_add(self.v_registers[register_y_index]);
        self.v_registers[FLAG_REGISTER_INDEX] = if carry { 1 } else { 0 };
        self.v_registers[register_x_index] = sum;
    }

    fn decrement_reg_with_reg(&mut self, register_x_index: usize, register_y_index: usize) {
        let (remainder, borrow) = self.v_registers[register_x_index].overflowing_sub(self.v_registers[register_y_index]);
        self.v_registers[FLAG_REGISTER_INDEX] = if borrow { 1 } else { 0 };
        self.v_registers[register_x_index] = remainder;
    }

    fn right_shift_reg(&mut self, register_index: usize) {
        // save rightmost bit to flag register
        self.v_registers[FLAG_REGISTER_INDEX] = self.v_registers[register_index] & 1;
        // save result of shifting to register
        self.v_registers[register_index] >>= 1;
    }

    fn decrement_reg_with_reg_opposite(&mut self, register_x_index: usize, register_y_index: usize) {
        let (remainder, borrow) = self.v_registers[register_y_index].overflowing_sub(self.v_registers[register_x_index]);
        self.v_registers[FLAG_REGISTER_INDEX] = if borrow { 1 } else { 0 };
        self.v_registers[register_x_index] = remainder;
    }

    fn left_shift_reg(&mut self, register_index: usize) {
        // save leftmost bit to flag register
        self.v_registers[FLAG_REGISTER_INDEX] = self.v_registers[register_index] & 0x80;
        // save result of shifting to register
        self.v_registers[register_index] <<= 1;
    }

    fn set_i_reg(&mut self, value: u16) {
        self.i_register = value;
    }

    fn goto_reg_0_plus_val(&mut self, value: u16) {
        self.program_counter = self.v_registers[0] as u16 + value;
    }

    fn rand_and_val(&mut self, register_index: usize, value: u8) {
        self.v_registers[register_index] = random::<u8>() & value;
    }

    fn draw_sprite(&mut self, register_x_index: usize, register_y_index: usize, height: u16) {
        // coords on the screen where a sprite will be drawn
        let x_coord = self.v_registers[register_x_index];
        let y_coord = self.v_registers[register_y_index];
        // 'height' is the sprite height in number of (pixel) rows
        let num_rows = height as u8;

        let mut bit_flipped = false;
        for row in 0..num_rows {
            // take a sprite row (sprite is saved in ram at address stored in i_register)
            let row_pixels = self.ram[(self.i_register + row as u16) as usize];
            for col in 0..8 {
                // test if the 'col'-th bit in row_pixels is 1 or 0 (if 1, we need to turn on that bit)
                if (row_pixels & (0x80 >> col)) != 0 {
                    // Sprites should wrap around screen, so apply modulo - x,y are coordinates where we are flipping a bit
                    let x = (x_coord + col) as usize % SCREEN_WIDTH;
                    let y = (y_coord + row) as usize % SCREEN_HEIGHT;

                    // 'index' is index where that bit is located in the screen array
                    let index = x + SCREEN_WIDTH * y;

                    // flipped |= self.screen[idx];
                    // self.screen[idx] ^= true;

                    // flip the bit
                    bit_flipped |= self.screen[index];
                    self.screen[index] ^= true;
                }
            }
        }

        // update VF (v[15]) register
        self.v_registers[FLAG_REGISTER_INDEX] = if bit_flipped { 1 } else { 0 };

    }

    fn skip_if_pressed(&mut self, register_index: usize) {
        if self.keys[self.v_registers[register_index] as usize] {
            self.program_counter += 2;
        }
    }

    fn skip_if_not_pressed(&mut self, register_index: usize) {
        if !self.keys[self.v_registers[register_index] as usize] {
            self.program_counter += 2;
        }
    }

    fn store_delay_timer(&mut self, register_index: usize) {
        self.v_registers[register_index] = self.delay_timer;
    }

    fn wait_for_key_press(&mut self, register_index: usize) {
        'outer: loop {
            for key_index in 0..self.keys.len() {
                if self.keys[key_index] {
                    self.v_registers[register_index] = key_index as u8;
                    break 'outer;
                }
            }
        }
    }

    fn set_delay_timer(&mut self, register_index: usize) {
        self.delay_timer = self.v_registers[register_index];
    }

    fn set_sound_timer(&mut self, register_index: usize) {
        self.sound_timer = self.v_registers[register_index];
    }

    fn increment_i_reg(&mut self, register_index: usize) {
        self.i_register = self.i_register.wrapping_add(self.v_registers[register_index] as u16);
    }

    fn set_i_reg_to_font_address(&mut self, register_index: usize) {
        self.i_register = (self.v_registers[register_index] * 5) as u16;
    }

    fn bcd_of_reg(&mut self, register_index: usize) {
        // convert it to decimal
        let mut num = self.v_registers[register_index];
        let digit1 = num / 100;
        num %= 100;
        let digit2 = num / 10;
        num %= 10;
        let digit3 = num;

        // store it into ram (where i_pointer is pointing at)
        self.ram[self.i_register as usize] = digit1;
        self.ram[(self.i_register + 1) as usize] = digit2;
        self.ram[(self.i_register + 2) as usize] = digit3;
    }

    fn store_reg_to_ram(&mut self, register_index: usize) {
        for index in 0..=register_index {
            self.ram[self.i_register as usize + index] = self.v_registers[index];
        }
    }

    fn store_ram_to_reg(&mut self, register_index: usize) {
        for index in 0..=register_index {
            self.v_registers[index] = self.ram[self.i_register as usize + index];
        }
    }
}