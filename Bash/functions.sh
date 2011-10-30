# This program is free software. It comes without any warranty, to
# the extent permitted by applicable law. You can redistribute it
# and/or modify it under the terms of the Do What The Fuck You Want
# To Public License, Version 2, as published by Sam Hocevar. See
# http://sam.zoy.org/wtfpl/COPYING for more details.



function print_layout {

	echo welcome to ducksboard
	echo "*******************************"
	echo 
	echo 
	echo  "(+) (*)                                         [______________]"
	echo  "+--------------++--------------++--------------++--------------+"
	echo  "|              ||              ||              ||              |"
	echo  "| (1) counter  ||  (2) gauge   ||  (3)    (4)  ||  (5) graph   |"
	echo  "|              ||              || gauge  gauge ||              |"
	echo  "+--------------++--------------++--------------++--------------+"
	echo  "+--------------++--------------++--------------+"
	echo  "|              ||              ||              |"
	echo  "|              ||              || (8) picture  |"
	echo  "|              ||              ||              |"
	echo  "| (6) timeline || (7) picture  |+--------------+"
	echo  "|              ||              |"
	echo  "|              ||              |"
	echo  "|              ||              |"
	echo  "+--------------++--------------+"
	echo
	echo -e "Which widget do you want to test? (1-8)"
	read widget_id
	input_data $widget_id
}




function check_global_variables {

	if [ "$COUNTER_URL_ID" = "" ]; then
	echo " ********* You need to set all variables before running this script!"
	kill -SIGINT $$
	fi

	if [ "$GAUGE_URL_ID" = "" ]; then
	echo " ********* You need to set all variables before running this script!"
	kill -SIGINT $$
	fi

	if [ "$GAUGE_LEFT_URL_ID" = "" ]; then
	echo " ********* You need to set all variables before running this script!"
	kill -SIGINT $$
	fi

	if [ "$GAUGE_RIGHT_URL_ID" = "" ]; then
	echo " ********* You need to set all variables before running this script!"
	kill -SIGINT $$
	fi

	if [ "$GRAPH_URL_ID" = "" ]; then
	echo " ********* You need to set all variables before running this script!"
	kill -SIGINT $$
	fi

	if [ "$TIMELINE_URL_ID" = "" ]; then
	echo " ********* You need to set all variables before running this script!"
	kill -SIGINT $$
	fi

	if [ "$PICTURE_URL_ID" = "" ]; then
	echo " ********* You need to set all variables before running this script!"
	kill -SIGINT $$
	fi

	if [ "$PICTURE_SMALL_URL_ID" = "" ]; then
	echo " ********* You need to set all variables before running this script!"
	kill -SIGINT $$
	fi

	if [ "$API_KEY" = "" ]; then
	echo " ********* You need to set all variables before running this script!"
	kill -SIGINT $$
	fi


}

function input_data {
	
	if [ $1 = 1 ]; then
	
    	echo -e "Which value do you want to push? [integer]"
		read value_to_push
		json="{\"value\": $value_to_push }"

		echo "**** Generated JSON ****"		
		echo $json		
		echo "************************"

		curl -u $API_KEY:ignored -d " $json "  https://push.ducksboard.com/values/$COUNTER_URL_ID/
		echo 

	elif [ $1 = 2 ]; then

    	echo -e "Which value do you want to push? [0.00-1.00]"
		read value_to_push
		json="{\"value\": $value_to_push }"

		echo "**** Generated JSON ****"		
		echo $json		
		echo "************************"

		curl -u $API_KEY:ignored -d " $json "  https://push.ducksboard.com/values/$GAUGE_URL_ID/
		echo
		
	elif [ $1 = 3 ]; then

    	echo -e "Which value do you want to push? [0.00-1.00]"
		read value_to_push
		json="{\"value\": $value_to_push }"
		
		echo "**** Generated JSON ****"		
		echo $json		
		echo "************************"

		curl -u $API_KEY:ignored -d " $json "  https://push.ducksboard.com/values/$GAUGE_LEFT_URL_ID/
		echo
		
	elif [ $1 = 4 ]; then

    	echo -e "Which value do you want to push? [0.00-1.00]"
		read value_to_push
		json="{\"value\": $value_to_push }"
		
		echo "**** Generated JSON ****"		
		echo $json		
		echo "************************"
		
		curl -u $API_KEY:ignored -d " $json "  https://push.ducksboard.com/values/$GAUGE_RIGHT_URL_ID/
    	echo
    	
 	elif [ $1 = 5 ]; then

    	echo -e "Which FIRST value do you want to push? [integer]"
		read value_to_push1
    	echo -e "Which SECOND value do you want to push? [integer]"
		read value_to_push2
				
		json="[{\"value\": $value_to_push1 }, {\"value\": $value_to_push2 }]"
		
		echo "**** Generated JSON ****"		
		echo $json		
		echo "************************"

		curl -u $API_KEY:ignored -d " $json "  https://push.ducksboard.com/values/$GRAPH_URL_ID/
    	echo
    	
	elif [ $1 = 6 ]; then

  		echo -e "What's the title of this event?"
		read title_to_push
    	echo -e "What's the content of this event?"
		read content_to_push
    	echo -e "Any URL to associate with this event? (including \"http://\")"
		read link_to_push
    	echo "Any image to associate this event?"
    	echo -e "(1) Red, (2) Green, (3) Orange, (4) Plus, (5) Minus, (6) Edit"
		read image_selection	
		image_to_push=$(choose_image $image_selection)

		json="{\"value\": {\"title\": \"$title_to_push\", \"image\": \"$image_to_push\", \"content\": \"$content_to_push\", \"link\": \"$link_to_push\"}}"

		echo "**** Generated JSON ****"		
		echo $json		
		echo "************************"
			

		curl -u $API_KEY:ignored -d " $json "  https://push.ducksboard.com/values/$TIMELINE_URL_ID/
		echo
		  	
 	elif [ $1 = 7 ]; then

    	echo -e "Where is located the image? [URL]"
		read source_to_push
		echo -e "Any caption to include?"
		read caption_to_push
		json="{\"value\": {\"source\": \"$source_to_push\", \"caption\": \"$caption_to_push\"}}"

		echo "**** Generated JSON ****"		
		echo $json		
		echo "************************"

		curl -u $API_KEY:ignored -d " $json "  https://push.ducksboard.com/values/$PICTURE_URL_ID/
		echo

	elif [ $1 = 8 ]; then

    	echo -e "Where is located the image? [URL]"
		read source_to_push
		echo -e "Any caption to include?"
		read caption_to_push
		json="{\"value\": {\"source\": \"$source_to_push\", \"caption\": \"$caption_to_push\"}}"

		echo "**** Generated JSON ****"		
		echo $json		
		echo "************************"

		curl -u $API_KEY:ignored -d " $json "  https://push.ducksboard.com/values/$PICTURE_SMALL_URL_ID/
		echo 

	else
	
		echo "Please, enter a correct value"
		echo -e "Which widget do you want to test? (1-8)"
		read widget_id
		input_data $widget_id
  	
    fi	
}

function choose_image {
			if [ -z "$1" ]; then 
		    	echo "http://a1.twimg.com/profile_images/1299623597/Ducksboard_small_logo_reasonably_small.png"
			elif [ $1 = 1 ]; then
    			echo "https://dashboard.ducksboard.com/static/img/timeline/red.gif"
			elif [ $1 = 2 ]; then
				echo "https://dashboard.ducksboard.com/static/img/timeline/green.gif"
			elif [ $1 = 3 ]; then
				echo "https://dashboard.ducksboard.com/static/img/timeline/orange.gif"
			elif [ $1 = 4 ]; then
				echo "https://dashboard.ducksboard.com/static/img/timeline/created.png"
			elif [ $1 = 5 ]; then
				echo "https://dashboard.ducksboard.com/static/img/timeline/deleted.png"
			elif [ $1 = 6 ]; then
				echo "https://dashboard.ducksboard.com/static/img/timeline/edited.png"
			else
				# By default, there's no image value, so we've added our logo to this script :)			
		    	echo "http://a1.twimg.com/profile_images/1299623597/Ducksboard_small_logo_reasonably_small.png"
		    fi 
		    
}



