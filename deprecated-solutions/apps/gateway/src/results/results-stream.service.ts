import { Injectable, MessageEvent } from '@nestjs/common';
import { Observable, ReplaySubject } from 'rxjs';

@Injectable()
export class ResultsStreamService {
  private subjects = new Map<string, ReplaySubject<MessageEvent>>();

  getStream(key: string): Observable<MessageEvent> {
    let subject = this.subjects.get(key);
    if (!subject) {
      subject = new ReplaySubject<MessageEvent>(1);
      this.subjects.set(key, subject);
    }
    return subject.asObservable();
  }

  emitResult(result: Record<string, unknown>): void {
    const key = (result['assignment_id'] as string) || (result['request_id'] as string);
    if (!key) return;
    const subject = this.subjects.get(key);
    if (subject) subject.next({ data: result } as MessageEvent);
  }
}
