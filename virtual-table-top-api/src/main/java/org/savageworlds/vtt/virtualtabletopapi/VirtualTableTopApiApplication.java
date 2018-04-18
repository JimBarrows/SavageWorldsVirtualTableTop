package org.savageworlds.vtt.virtualtabletopapi;

import org.savageworlds.vtt.virtualtabletopapi.models.User;
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

}
