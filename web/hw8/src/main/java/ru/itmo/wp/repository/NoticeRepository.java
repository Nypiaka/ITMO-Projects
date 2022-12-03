package ru.itmo.wp.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import ru.itmo.wp.domain.Notice;

public interface NoticeRepository extends JpaRepository<Notice, Long> {
    
}
