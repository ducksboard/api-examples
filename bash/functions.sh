# This program is free software. It comes without any warranty, to
# the extent permitted by applicable law. You can redistribute it
# and/or modify it under the terms of the Do What The Fuck You Want
# To Public License, Version 2, as published by Sam Hocevar. See
# http://sam.zoy.org/wtfpl/COPYING for more details.

function print_layout {

	echo "welcome to the ducksboard API bash example :)"
	echo "*************************************************************************"
	echo 
	echo  "(+) (*)                                               [________________]"
	echo  "+----------------++----------------++----------------++----------------+"
	echo  "|                ||                ||                ||                |"
	echo  "|  (1) counter   ||   (2) gauge    ||   (3)    (4)   ||   (5) graph    |"
	echo  "|                ||                ||  gauge  gauge  ||                |"
	echo  "+----------------++----------------++----------------++----------------+"
	echo  "+----------------++----------------++----------------++----------------+"
	echo  "|                ||                ||                ||_____       bars|"
	echo  "|                ||                ||   (8) image    || (9) |_____     |"
	echo  "|                ||                ||                ||     |(10) |(11)|"
	echo  "|  (6) timeline  ||   (7) image    |+----------------++----------------+"
	echo  "|                ||                |+----------------++----------------+"
	echo  "|                ||                ||   ____  ____   || __  __  __  __ |"
	echo  "|                ||                ||  |(12)||(13)|  |||14||15||16||17||"
	echo  "|                ||                ||    \/    \/    |||__||__||__||__||"
	echo  "+----------------++----------------++----------------++----------------+"
	echo
	echo -e "Which widget data-source do you want to test? (1-17)"
	read widget_id
	input_data $widget_id
}


function input_data {
	
	if [ $1 = 1 ]; then
		
		if [ -z "$COUNTER_URL_ID" -o -z "$API_KEY" ]; then
			echo "You need to set COUNTER_URL_ID and API_KEY at \"vars.sh\"  before using this widget!"
			kill -SIGINT $$		
		fi
		
    	echo -e "Which value do you want to push? [integer]"
		read value_to_push
		json="{\"value\": $value_to_push }"

		echo "**** Generated JSON ****"		
		echo $json		
		echo "************************"

		curl -u $API_KEY:ignored -d " $json "  https://push.ducksboard.com/values/$COUNTER_URL_ID/
		echo 

	elif [ $1 = 2 ]; then

		if [ -z "$GAUGE_URL_ID" -o -z "$API_KEY" ]; then
			echo "You need to set GAUGE_URL_ID and API_KEY at \"vars.sh\"  before using this widget!"
			kill -SIGINT $$		
		fi

    	echo -e "Which value do you want to push? [0.00-1.00]"
		read value_to_push
		json="{\"value\": $value_to_push }"

		echo "**** Generated JSON ****"		
		echo $json		
		echo "************************"

		curl -u $API_KEY:ignored -d " $json "  https://push.ducksboard.com/values/$GAUGE_URL_ID/
		echo
		
	elif [ $1 = 3 ]; then

		if [ -z "$GAUGE_LEFT_URL_ID" -o -z "$API_KEY" ]; then
			echo "You need to set GAUGE_LEFT_URL_ID and API_KEY at \"vars.sh\"  before using this widget!"
			kill -SIGINT $$		
		fi
		
    	echo -e "Which value do you want to push? [0.00-1.00]"
		read value_to_push
		json="{\"value\": $value_to_push }"
		
		echo "**** Generated JSON ****"		
		echo $json		
		echo "************************"

		curl -u $API_KEY:ignored -d " $json "  https://push.ducksboard.com/values/$GAUGE_LEFT_URL_ID/
		echo
		
	elif [ $1 = 4 ]; then

		if [ -z "$GAUGE_RIGHT_URL_ID" -o -z "$API_KEY" ]; then
			echo "You need to set GAUGE_RIGHT_URL_ID and API_KEY at \"vars.sh\"  before using this widget!"
			kill -SIGINT $$		
		fi
		
    	echo -e "Which value do you want to push? [0.00-1.00]"
		read value_to_push
		json="{\"value\": $value_to_push }"
		
		echo "**** Generated JSON ****"		
		echo $json		
		echo "************************"
		
		curl -u $API_KEY:ignored -d " $json "  https://push.ducksboard.com/values/$GAUGE_RIGHT_URL_ID/
    	echo
    	
 	elif [ $1 = 5 ]; then

		if [ -z "$GRAPH_URL_ID" -o -z "$API_KEY" ]; then
			echo "You need to set GRAPH_URL_ID and API_KEY at \"vars.sh\"  before using this widget!"
			kill -SIGINT $$		
		fi
		
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

		if [ -z "$TIMELINE_URL_ID" -o -z "$API_KEY" ]; then
			echo "You need to set TIMELINE_URL_ID and API_KEY at \"vars.sh\"  before using this widget!"
			kill -SIGINT $$		
		fi
		
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

		if [ -z "$PICTURE_URL_ID" -o -z "$API_KEY" ]; then
			echo "You need to set PICTURE_URL_ID and API_KEY at \"vars.sh\"  before using this widget!"
			kill -SIGINT $$		
		fi
		
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

		if [ -z "$PICTURE_SMALL_URL_ID" -o -z "$API_KEY" ]; then
			echo "You need to set PICTURE_SMALL_URL_ID and API_KEY at \"vars.sh\"  before using this widget!"
			kill -SIGINT $$		
		fi
		
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
		
	elif [ $1 = 9 ]; then

		if [ -z "$BAR_1_URL_ID" -o -z "$API_KEY" ]; then
			echo "You need to set BAR_1_URL_ID and API_KEY at \"vars.sh\"  before using this widget!"
			kill -SIGINT $$		
		fi
			
    	echo -e "Which value do you want to push? [integer]"
		read value_to_push
		json="{\"value\": $value_to_push }"

		echo "**** Generated JSON ****"		
		echo $json		
		echo "************************"

		curl -u $API_KEY:ignored -d " $json "  https://push.ducksboard.com/values/$BAR_1_URL_ID/
		echo 		 
		
	elif [ $1 = 10 ]; then

		if [ -z "$BAR_2_URL_ID" -o -z "$API_KEY" ]; then
			echo "You need to set BAR_2_URL_ID and API_KEY at \"vars.sh\"  before using this widget!"
			kill -SIGINT $$		
		fi
			
    	echo -e "Which value do you want to push? [integer]"
		read value_to_push
		json="{\"value\": $value_to_push }"

		echo "**** Generated JSON ****"		
		echo $json		
		echo "************************"

		curl -u $API_KEY:ignored -d " $json "  https://push.ducksboard.com/values/$BAR_2_URL_ID/
		echo 		 
		
	elif [ $1 = 11 ]; then

		if [ -z "$BAR_3_URL_ID" -o -z "$API_KEY" ]; then
			echo "You need to set BAR_3_URL_ID and API_KEY at \"vars.sh\"  before using this widget!"
			kill -SIGINT $$		
		fi
			
    	echo -e "Which value do you want to push? [integer]"
		read value_to_push
		json="{\"value\": $value_to_push }"

		echo "**** Generated JSON ****"		
		echo $json		
		echo "************************"

		curl -u $API_KEY:ignored -d " $json "  https://push.ducksboard.com/values/$BAR_3_URL_ID/
		echo 		 
		
	elif [ $1 = 12 ]; then

		if [ -z "$PIN_1_URL_ID" -o -z "$API_KEY" ]; then
			echo "You need to set PIN_1_URL_ID and API_KEY at \"vars.sh\"  before using this widget!"
			kill -SIGINT $$		
		fi
			
    	echo -e "Which value do you want to push? [integer]"
		read value_to_push
		json="{\"value\": $value_to_push }"

		echo "**** Generated JSON ****"		
		echo $json		
		echo "************************"

		curl -u $API_KEY:ignored -d " $json "  https://push.ducksboard.com/values/$PIN_1_URL_ID/
		echo 		 
		
	elif [ $1 = 13 ]; then

		if [ -z "$PIN_2_URL_ID" -o -z "$API_KEY" ]; then
			echo "You need to set PIN_2_URL_ID and API_KEY at \"vars.sh\"  before using this widget!"
			kill -SIGINT $$		
		fi
			
    	echo -e "Which value do you want to push? [integer]"
		read value_to_push
		json="{\"value\": $value_to_push }"

		echo "**** Generated JSON ****"		
		echo $json		
		echo "************************"

		curl -u $API_KEY:ignored -d " $json "  https://push.ducksboard.com/values/$PIN_2_URL_ID/
		echo 		 
		
	elif [ $1 = 14 ]; then

		if [ -z "$BOX_1_URL_ID" -o -z "$API_KEY" ]; then
			echo "You need to set BOX_1_URL_ID and API_KEY at \"vars.sh\"  before using this widget!"
			kill -SIGINT $$		
		fi
			
    	echo -e "Which value do you want to push? [integer]"
		read value_to_push
		json="{\"value\": $value_to_push }"

		echo "**** Generated JSON ****"		
		echo $json		
		echo "************************"

		curl -u $API_KEY:ignored -d " $json "  https://push.ducksboard.com/values/$BOX_1_URL_ID/
		echo 		 
		
	elif [ $1 = 15 ]; then

		if [ -z "$BOX_2_URL_ID" -o -z "$API_KEY" ]; then
			echo "You need to set BOX_2_URL_ID and API_KEY at \"vars.sh\"  before using this widget!"
			kill -SIGINT $$		
		fi
			
    	echo -e "Which value do you want to push? [integer]"
		read value_to_push
		json="{\"value\": $value_to_push }"

		echo "**** Generated JSON ****"		
		echo $json		
		echo "************************"

		curl -u $API_KEY:ignored -d " $json "  https://push.ducksboard.com/values/$BOX_2_URL_ID/
		echo 		 
		
	elif [ $1 = 16 ]; then

		if [ -z "$BOX_3_URL_ID" -o -z "$API_KEY" ]; then
			echo "You need to set BOX_3_URL_ID and API_KEY at \"vars.sh\"  before using this widget!"
			kill -SIGINT $$		
		fi
			
    	echo -e "Which value do you want to push? [integer]"
		read value_to_push
		json="{\"value\": $value_to_push }"

		echo "**** Generated JSON ****"		
		echo $json		
		echo "************************"

		curl -u $API_KEY:ignored -d " $json "  https://push.ducksboard.com/values/$BOX_3_URL_ID/
		echo 		 
		
	elif [ $1 = 17 ]; then

		if [ -z "$BOX_4_URL_ID" -o -z "$API_KEY" ]; then
			echo "You need to set BOX_4_URL_ID and API_KEY at \"vars.sh\"  before using this widget!"
			kill -SIGINT $$		
		fi
			
    	echo -e "Which value do you want to push? [integer]"
		read value_to_push
		json="{\"value\": $value_to_push }"

		echo "**** Generated JSON ****"		
		echo $json		
		echo "************************"

		curl -u $API_KEY:ignored -d " $json "  https://push.ducksboard.com/values/$BOX_4_URL_ID/
		echo 		 

	else
	
		echo "Please, enter a correct value"
		echo -e "Which widget do you want to test? (1-17)"
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



