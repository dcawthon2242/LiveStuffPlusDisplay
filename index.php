<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Stuff+ Display</title>
    <style>
        body {
            background-color: black;
            color: limegreen;
            font-family: monospace;
            font-size: 30px;
            text-align: center;
            margin: 0;
            padding: 0;
        }
        #output {
            font-size: 120px;
            margin-top: 40px;
        }
        form {
            margin-top: 20px;
        }
        input {
            font-size: 24px;
            margin: 5px;
        }
        label {
            display: inline-block;
            width: 180px;
            text-align: right;
            margin-right: 10px;
        }
    </style>
</head>
<body>
    <h1>Stuff+ Display</h1>

    <form method="post">
        <label>Release Speed:</label><input type="text" name="release_speed" value="92.1"><br>
        <label>Spin Rate:</label><input type="text" name="release_spin_rate" value="2200"><br>
        <label>Spin Axis:</label><input type="text" name="spin_axis" value="180"><br>
        <label>Release Pos X:</label><input type="text" name="release_pos_x" value="-1.5"><br>
        <label>Release Pos Z:</label><input type="text" name="release_pos_z" value="6.0"><br>
        <label>pfx Z:</label><input type="text" name="pfx_z" value="-1.2"><br>
        <label>pfx X:</label><input type="text" name="pfx_x" value="0.8"><br>
        <br>
        <input type="submit" value="Predict Stuff+">
    </form>

    <div id="output">
        <?php
        if ($_SERVER["REQUEST_METHOD"] == "POST") {
            $fields = ["release_speed", "release_spin_rate", "spin_axis", "release_pos_x", "release_pos_z", "pfx_z", "pfx_x"];
            $args = [];

            foreach ($fields as $field) {
                if (!isset($_POST[$field]) || !is_numeric($_POST[$field])) {
                    echo "<div>Missing or invalid input: $field</div>";
                    exit;
                }
                $args[] = floatval($_POST[$field]);
            }

            $args_string = implode(" ", array_map("escapeshellarg", $args));
            $rscript_path = "/var/www/html/predict_unified_model.R";
            $env = "R_LIBS_USER=/home/fullertonbaseball/R/x86_64-pc-linux-gnu-library/4.3";
            $cmd = "$env Rscript $rscript_path $args_string";

            exec($cmd, $output, $return_code);

            if ($return_code !== 0 || empty($output)) {
                echo "<div>Stuff+ Unavailable</div>";
                echo "<div style='font-size: 20px; margin-top: 20px;'>";
                echo "<strong>Error Code:</strong> $return_code<br>";
                echo "<strong>Command:</strong><br><code>$cmd</code><br>";

                if (!empty($output)) {
                    echo "<strong>R Output:</strong><pre>" . implode("\n", $output) . "</pre>";
                }
                echo "</div>";
            } else {
                echo round(floatval($output[0]));
            }
        }
        ?>
    </div>
</body>
</html>
