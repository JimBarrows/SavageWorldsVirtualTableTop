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

@SpringBootApplication
public class VirtualTableTopApiApplication {

	public static void main(String[] args) {
		SpringApplication.run(VirtualTableTopApiApplication.class, args);
	}

	@Bean
	public CommandLineRunner users(UserRepository userRepository) {
		return (args) -> {
			userRepository.save(new User("admin", new BCryptPasswordEncoder().encode("admin")));
		};
	}

	@Bean
	public CommandLineRunner plotPoints(PlotPointRepository plotPointRepository) {
		return (args) -> {
			PlotPoint plotPoint = new PlotPoint();
			plotPoint.setName("This is a plot point");
			plotPoint.setDescription("This is it's description");
			plotPoint.getRaces().add(getRace());
			plotPointRepository.save(plotPoint);
		};
	}

	private Race getRace() {
		Race race = new Race();
		race.setName("Human");
		race.setDescription("Human description");
		race.getAbilities().add(new RacialAbility("Extra Edge", "Gets and extra edge", 2));
		return race;
	}
}
