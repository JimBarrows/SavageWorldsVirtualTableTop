package org.savageworlds.vtt.virtualtabletopapi.repositories;

import org.savageworlds.vtt.virtualtabletopapi.models.User;
import org.springframework.data.jpa.repository.JpaRepository;

public interface UserRepository extends JpaRepository<User, Long> {

	User findByUsername(String username);
}
