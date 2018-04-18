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

import static java.lang.String.format;

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
			plotPoint.getRaces().add(getRace("Human"));
			plotPoint.getRaces().add(getRace("Elf"));
			plotPointRepository.save(plotPoint);
		};
	}

	private Race getRace(String name) {
		Race race = new Race();
		race.setName(name);
		race.setDescription(format("%s description", name));
		race.getAbilities().add(new RacialAbility(format("%s edge", name), format("%s edge description", name), 2));
		return race;
	}
}
