transcript on

# Создаем собственную библиотеку из свои модулей
vlib work

# Путь к скомпилированным библиотекам Alter'ы
# 	D:\Gorkovenko\Library\ModelSim\altera\verilog\

# Путь к собственным библиотечным файлам
#	D:\Gorkovenko\Library\Quartus\Main_LIB\

#vcom -work work D:/Program_Files/ModelSim16_1/modelsim_ase/altera/verilog/eda/sim_lib/altera_mf.vhd

# Компилируем файлы
#	Тестбенч и основные файлы
vlog -sv testbench.sv
vlog -sv ../DFF.sv
vlog -sv ../DFFE.sv
vlog -sv ../SRFF.sv
vlog -sv ../SRFFE.sv
# Запускаем симулятор 
vsim -c -voptargs="+acc" -L D:/Program_Files/Quartus18/modelsim_ase/altera/verilog/altera_mf -L D:/Program_Files/Quartus18/modelsim_ase/altera/verilog/cyclonev -L D:/Program_Files/Quartus18/modelsim_ase/altera/verilog/220model work.testbench 


# Выводим все сигналы
add wave sim:/testbench/*

# run the simulation
run 100 ns 


# expand the signals time diagram
wave zoom full
