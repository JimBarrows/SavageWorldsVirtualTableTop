package org.savageworlds.vtt.virtualtabletopapi;

import org.savageworlds.vtt.virtualtabletopapi.models.PlotPoint;
import org.savageworlds.vtt.virtualtabletopapi.models.Race;
import org.savageworlds.vtt.virtualtabletopapi.models.RacialAbility;
import org.savageworlds.vtt.virtualtabletopapi.models.User;
import org.savageworlds.vtt.virtualtabletopapi.repositories.PlotPointRepository;
import org.savageworlds.vtt.virtualtabletopapi.repositories.UserRepository;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Set;

@SpringBootApplication
public class VirtualTableTopApiApplication {

	public static void main(String... args) {
		SpringApplication.run(VirtualTableTopApiApplication.class, args);
	}

	@Bean
	public CommandLineRunner users(UserRepository userRepository) {
		return (args) -> userRepository.save(new User("admin", new BCryptPasswordEncoder().encode("admin")));
	}

	@Bean
	public CommandLineRunner plotPoints(PlotPointRepository plotPointRepository) {
		return (args) -> {
			for (int i = 0; i < 20; i++) {
				plotPointRepository.save(plotPoint(i));
			}
		};
	}

	private PlotPoint plotPoint(int number) {
		PlotPoint plotPoint = new PlotPoint();
		plotPoint.setName("Test plot point " + number);
		plotPoint.setDescription("Test plot point description " + number);
		plotPoint.getRaces().addAll(races(2));
		return plotPoint;
	}

	private Set<Race> races(final int numberToCreate) {
		Set<Race> races = new LinkedHashSet<>();
		for( int i =0;i<numberToCreate;i++ ) {
			Race race = new Race();
			race.setName("Race Name " + i);
			race.setDescription("Race Description " + i);
			race.getAbilities().addAll( racialAbilities(2));
			races.add(race);
		}
		return races;
	}

	private Set<RacialAbility> racialAbilities(final int numberToCreate) {
		Set<RacialAbility> racialAbilities = new LinkedHashSet<>();
		for( int i =0; i<numberToCreate;i++) {
			RacialAbility racialAbility = new RacialAbility();
			racialAbility.setName("Racial Ability " + i);
			racialAbility.setDescription("Racial Ability Description " +i);
			racialAbility.setPoints( i);
			racialAbilities.add(racialAbility);
		}
		return racialAbilities;
	}
}
