for i in $(ls ../experiments/)
do
  if [ -d "../experiments/$i/calculations" ] && [ -d "../experiments/$i/measures" ]
  then
    gnuplot -e "plotname='../experiments/$i/$i.png'; titlename='Trilateration';
    calcfiles='
      ../experiments/$i/calculations/position_sensor_fusion@my_grisp_board_1.1.csv
      ../experiments/$i/calculations/position_sensor_fusion@my_grisp_board_2.1.csv
      ../experiments/$i/calculations/position_sensor_fusion@my_grisp_board_3.1.csv
      ../experiments/$i/calculations/position_sensor_fusion@my_grisp_board_4.1.csv
      ';

      posfiles='
      ../experiments/$i/measures/pos_sensor_fusion@my_grisp_board_1.1.csv
      ../experiments/$i/measures/pos_sensor_fusion@my_grisp_board_2.1.csv
      ../experiments/$i/measures/pos_sensor_fusion@my_grisp_board_3.1.csv
      ../experiments/$i/measures/pos_sensor_fusion@my_grisp_board_4.1.csv
      '" plot_config.plg
  fi
done